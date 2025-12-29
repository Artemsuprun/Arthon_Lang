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
       | NAME "(" explist ")"   -> func_call_stmt

  COMMENT: /:[^\n]*/

  CHAR: /'[^']'/

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


# Global name environment for the interpreter.
env = Env()

# Interpreter
#
@v_args(inline=True)
class Eval(Interpreter):
    # represents an integer
    def num(self, num):
        if '.' in num:
            return float(num)
        return int(num)

    # represents a value held in the name of an object
    def name(self, name):
        return env.lookup(name)
    
    # represents a character
    def char(self, c):
        return c[1]
    
    # represents a none value
    def none(self, _):
        return None

    # converts a number to negative
    def neg(self, val):
        num = self.visit(val)
        if not isinstance(num, int):
            raise Exception("Type error: negation requires an integer")
        return -num

    # arithmetic operations
    def div(self, l, r): 
        return self.visit(l) // self.visit(r)
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
        global env
        old_env = env
        env = Env(parent=old_env)
        for stmt in stmts:
            self.visit(stmt)
        env = old_env
    
    # function definition
    def lambda_expr(self, x, e):
        return Closure(x, e, env)

    # function call
    def func_call_stmt(self, fname, args=None):
        global env
        closure = env.lookup(fname)
        args = self.visit(args)

        if len(args) != len(closure.params):
            raise Exception("Argument count mismatch!")
        
        old_env = env
        env = Env(parent=closure.env)

        for name, value in zip(closure.params, args):
            env.define(name, value)
        
        try:
            self.visit(closure.body)
            result = None
        except Return as r:
            result = r.value
        finally:
            env = old_env
        
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
        global env
        param = self.visit(param)
        closure = Closure(param, body, env)
        env.define(fname, closure)

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
        print(self.visit(expr))

    # variable declaration
    def var_decl_stmt(self, name, expr):
        global env
        env.define(name, self.visit(expr))

    # variable assignment
    def assign_stmt(self, name, expr):
        global env
        env.update(name, self.visit(expr))

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
        global env
        old_env = env
        env = Env(parent=old_env)
        env.define(id, 0)
        for i in range(r[0], r[1], steps):
            env.update(id, i)
            self.visit(stmt)
        env = old_env
    
    # range block
    def range_block(self, start, end):
        return (int(start), int(end))



# Main function
#
import sys
def main():
    try:
        prog = sys.stdin.read()
        tree = parser.parse(prog)
        print(prog, end="")
        #TypeCheck().visit(tree)
        Eval().visit(tree)
    except Exception as e:
        print(e)

if __name__ == "__main__":
    main()

