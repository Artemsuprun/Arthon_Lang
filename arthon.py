# Author:   Artem Suprun
# Date:     12/16/2025

from lark import Lark, v_args
from lark.visitors import Interpreter
from lark.visitors import Visitor

debug = True

grammar = r"""
  ?start: stmt*

  ?stmt: simple_stmt ";"
       | compound_stmt

  ?simple_stmt: "var" NAME "=" expr     -> var_decl_stmt
       | NAME "=" expr                  -> assign_stmt
       | "print" "(" expr ")"           -> print_stmt
       | return
       | expr

  ?return: "return" expr        -> return_stmt

  ?compound_stmt: "for" NAME "in" range stmt    -> for_stmt
       | "if" "(" expr ")" stmt ["else" stmt]   -> if_stmt
       | "while" "(" expr ")" stmt              -> while_stmt
       | "func" NAME "(" parlist ")" body       -> func_def_stmt
       | block

  ?block: "{" stmt* "}"         -> block

  ?body: "{" stmt* "}"          -> body

  ?range: "[" INT ".." INT "]"          -> range_block

  ?parlist: [NAME ("," NAME)*]          -> param_list

  ?expr: "lambda" NAME ":" expr         -> lambda_expr
       | coper 

  ?explist: [expr ("," expr)*]      -> arg_list

  ?coper: aoper "<" aoper           -> ltop
        | aoper ">" aoper           -> gtop
        | aoper "<=" aoper          -> leop
        | aoper ">=" aoper          -> geop
        | aoper "==" aoper          -> eqop
        | aoper "!=" aoper          -> neop
        | aoper

  ?aoper: aoper "+" moper       -> add
        |  aoper "-" moper      -> sub
        |  moper         

  ?moper: moper "*" unary       -> mul
        | moper "/" unary       -> div
        | unary

  ?unary: "-" atom              -> neg
        | "!" atom              -> notop
        | preop

  ?preop: "++" atom
        | "--" atom
        | atom

  ?atom: "(" expr ")"
       | NAME                   -> name
       | (INT | FLOAT)          -> num
       | CHAR                   -> char
       | NONE                   -> none
       | BOOL                   -> bool
       | NAME "(" explist ")"   -> func_call_stmt

  COMMENT: /:[^\n]*/

  CHAR: /'[^']'/

  BOOL: "true" | "false"

  NONE: "none"

  %import common.CNAME      -> NAME
  %import common.INT        -> INT
  %import common.FLOAT      -> FLOAT
  %import common.WS

  %ignore WS
  %ignore COMMENT
"""

parser = Lark(grammar, parser='lalr')

# INTERPRETER ---------------------------------------------

# Name environment
#
class Env():
    def __init__(self, parent=None):
        # using a dict to map names to values
        self.values = {}
        # used for iterating through multiple name environments
        self.parent = parent

    # define a name to hold a value
    def define(self, name, value):
        if name in self.values:
            raise Exception(f"The name '{name}' is already defined!")
        self.values[name] = value

    # look up the value held in a name
    def lookup(self, name):
        if name in self.values:
            return self.values[name]
        # look into the parent node
        if self.parent:
            return self.parent.lookup(name)
        raise Exception(f"Undefined Variable '{name}'")

    # update the value held in a name
    def update(self, name, value):
        if name in self.values:
            self.values[name] = value
            return
        # look into the parent node
        if self.parent:
            self.parent.update(name, value)
            return
        raise Exception(f"Undefined Variable named '{name}'")


# Closure
#
class Closure():
    # function closure
    def __init__(self, params, body, env):
        # holds functions params
        self.params = params
        # holds function body
        self.body = body
        # holds function name environment
        self.env = env


# Return 
#
class Return(Exception):
    def __init__(self, value):
        self.value = value


# Interpreter
#
@v_args(inline=True)
class Eval(Interpreter):
    def __init__(self):
        self.env = Env()
    
    # represents an integer
    def num(self, num):
        if '.' in num:
            return float(num)
        return int(num)

    # represents a value held in the name of an object
    def name(self, name):
        return self.env.lookup(name)
    
    # represents a character
    def char(self, c):
        return c[1]
    
    # represents a boolean
    def bool(self, b):
        return 1 if b == "true" else 0
    
    # represents a none value
    def none(self, _):
        return None

    # converts a number to negative
    def neg(self, val):
        return -self.visit(val)

    # arithmetic operations
    def div(self, l, r): 
        return self.visit(l) / self.visit(r)
    # multiplication operator
    def mul(self, l, r): 
        return self.visit(l) * self.visit(r)
    # subtraction operator
    def sub(self, l, r): 
        return self.visit(l) - self.visit(r)
    # addition operator
    def add(self, l, r): 
        return self.visit(l) + self.visit(r)

    # less than operator
    def ltop(self, l, r):
        return self.visit(l) < self.visit(r)
    # greater than operator
    def gtop(self, l, r):
        return self.visit(l) > self.visit(r)
    # less than or equal operator
    def leop(self, l, r):
        return self.visit(l) <= self.visit(r)
    # greater than or equal operator
    def geop(self, l, r):
        return self.visit(l) >= self.visit(r)
    # equality operator
    def eqop(self, l, r):
        return self.visit(l) == self.visit(r)
    # not equal operator
    def neop(self, l, r):
        return self.visit(l) != self.visit(r)
    
    # statements
    def block(self, *stmts):
        old_env = self.env
        self.env = Env(parent=old_env)
        for stmt in stmts:
            self.visit(stmt)
        self.env = old_env
    
    # function definition
    def lambda_expr(self, name, exprs):
        return Closure(name, exprs, self.env)

    # function call
    def func_call_stmt(self, fname, args=None):
        closure = self.env.lookup(fname)
        args = self.visit(args)

        assert len(args) == len(closure.params)
        
        old_env = self.env
        self.env = Env(parent=closure.env)

        for name, value in zip(closure.params, args):
            self.env.define(name, value)
        
        try:
            self.visit(closure.body)
            result = None
        except Return as r:
            result = r.value
        finally:
            self.env = old_env

        return result
    
    # expression list
    def arg_list(self, *exprs):
        args = []
        if exprs[0] is None:
            return args
        for expr in exprs:
            args.append(self.visit(expr))
        return args
    
    # function declaration
    def func_def_stmt(self, fname, param, body):
        param = self.visit(param)
        closure = Closure(param, body, self.env)
        self.env.define(fname, closure)

    # parameter list
    def param_list(self, *params):
        args = []
        if params[0] is None:
            return args
        for param in params:
            args.append(param)
        return args
    
    # function body
    def body(self, *stmts):
        try:
            self.block(*stmts)
            return None
        except Return:
            raise
    
    # return statement
    def return_stmt(self, expr=None):
        value = self.visit(expr) if expr else None
        raise Return(value)

    # print statement
    def print_stmt(self, expr):
        txt = self.visit(expr)
        if txt is not None:
            print(txt)

    # variable declaration
    def var_decl_stmt(self, name, expr):
        self.env.define(name, self.visit(expr))

    # variable assignment
    def assign_stmt(self, name, expr):
        self.env.update(name, self.visit(expr))

    # if statement
    def if_stmt(self, expr, stmt_1, stmt_2):
        if self.visit(expr) != 0:
            self.visit(stmt_1)
        elif stmt_2:
            self.visit(stmt_2)
    
    # while statement
    def while_stmt(self, expr, stmt):
        while self.visit(expr) != 0:
            self.visit(stmt)

    # for statement
    def for_stmt(self, id, r, stmt):
        r = self.visit(r)
        if r[0] > r[1]:
            steps = -1
        else:
            steps = 1
        # for loop's own scope
        old_env = self.env
        self.env = Env(parent=old_env)
        self.env.define(id, 0)
        for i in range(r[0], r[1], steps):
            self.env.update(id, i)
            self.visit(stmt)
        self.env = old_env
    
    # range block
    def range_block(self, start, end):
        return (int(start), int(end))


# TYPE CHECKER --------------------------------------------

# TYPES
#
class Type:
    pass
class IntType(Type):
    def __repr__(self):
        return "Int"
class BoolType(Type):
    def __repr__(self):
        return "Bool"
class CharType(Type):
    def __repr__(self):
        return "Char"
class NoneType(Type):
    def __repr__(self):
        return "None"
class FuncType(Type):
    def __init__(self, params, ret):
        self.params = params
        self.ret = ret
    def __repr__(self):
        return f"({', '.join(map(str, self.params))}) -> {self.ret}"

class TypeEnv:
    def __init__(self, parent=None):
        self.types = {}
        self.parent = parent
    
    def define(self, name, value_type):
        if name in self.types:
            raise Exception(f"Type error: '{name}' already defined")
        self.types[name] = value_type
    
    def lookup(self, name):
        if name in self.types:
            return self.types[name]
        if self.parent:
            return self.parent.lookup(name)
        raise Exception(f"Type error: undefined variable '{name}'")

# Predefined types instances
INT = IntType()
BOOL = BoolType()
CHAR = CharType()
NONE = NoneType()

@v_args(inline=True)
class TypeCheck(Visitor):
    def __init__(self):
        self.env = TypeEnv()

    def num(self, _):
        return INT
    
    def char(self, _):
        return CHAR
    
    def none(self, _):
        return NONE
    
    def name(self, name):
        return self.env.lookup(name)
    
    def add(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return INT
    
    def sub(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return INT
    
    def mul(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return INT
    
    def div(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return INT
    
    def _expect_type(self, expr, expected_type):
        actual_type = self.visit(expr)
        if actual_type != expected_type:
            raise Exception(f"Type error: expected {expected_type}, got {actual_type}")
    
    def ltop(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return BOOL
    
    def gtop(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return BOOL
    
    def leop(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return BOOL
    
    def geop(self, l, r):
        self._expect_type(l, INT)
        self._expect_type(r, INT)
        return BOOL
    
    def eqop(self, l, r):
        l_type = self.visit(l)
        r_type = self.visit(r)
        if l_type != r_type:
            raise Exception(f"Type error: cannot compare {l_type} with {r_type}")
        return BOOL
    
    def neop(self, l, r):
        l_type = self.visit(l)
        r_type = self.visit(r)
        if l_type != r_type:
            raise Exception(f"Type error: cannot compare {l_type} with {r_type}")
        return BOOL
    
    def var_decl_stmt(self, name, expr):
        value_type = self.visit(expr)
        self.env.define(name, value_type)

    def assign_stmt(self, name, expr):
        value_type = self.visit(expr)
        var_type = self.env.lookup(name)
        if value_type != var_type:
            raise Exception(f"Type error: cannot assign {value_type} to {var_type}")
        
    def block(self, *stmts):
        old_env = self.env
        self.env = TypeEnv(parent=old_env)
        for stmt in stmts:
            self.visit(stmt)
        self.env = old_env

    def func_def_stmt(self, name, params, body):
        param_types = [INT for _ in params]  # Assuming all parameters are of type INT for simplicity

        func_type = FuncType(param_types, NONE)
        self.env.define(name, func_type)

        old_env = self.env
        self.env = TypeEnv(parent=old_env)
        for param, param_type in zip(params, param_types):
            self.env.define(param, param_type)
        
        body_ret = self.visit(body)
        func_type.ret = body_ret

        self.env = old_env
    
    def return_stmt(self, expr):
        return self.visit(expr) if expr else NONE
    
    def body(self, *stmts):
        ret_type = NONE
        for stmt in stmts:
            stmt_type = self.visit(stmt)
            if stmt_type != NONE:
                ret_type = stmt_type
        return ret_type
    
    def func_call_stmt(self, fname, args):
        func_type = self.visit(fname)
        if not isinstance(func_type, FuncType):
            raise Exception("Type error: trying to call a non-function")
        
        arg_types = [self.visit(arg) for arg in args]
        if len(arg_types) != len(func_type.params):
            raise Exception("Argument count mismatch!")
        for arg_type, param_type in zip(arg_types, func_type.params):
            if arg_type != param_type:
                raise Exception("Argument type mismatch!")
        return func_type.ret


# Main function
#
import sys
def main():
    try:
        prog = sys.stdin.read()
        tree = parser.parse(prog)
        print(prog, end="")
        #TypeCheck().visit(tree) The type checker is currently disabled for fixing purposes
        Eval().visit(tree)
    except Exception as e:
        print(e)

if __name__ == "__main__":
    main()

