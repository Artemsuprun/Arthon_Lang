# Author:   Artem Suprun
# Date:     12/16/2025

from lark import Lark, v_args
from lark.visitors import Interpreter
import copy

debug = True

grammar = r"""
  ?start: stmt*

  ?stmt: simple_stmt ";"
       | compound_stmt

  ?simple_stmt: "var" NAME "=" expr       -> var_decl_stmt
       | NAME "=" expr                    -> assign_stmt
       | "print" "(" expr ")"           -> print_stmt
       | NAME "(" explist ")"             -> func_call_stmt
       | "return" expr                  -> return_stmt

  ?compound_stmt: "for" NAME "in" range stmt      -> for_stmt
       | "if" "(" expr ")" stmt ["else" stmt]   -> if_stmt
       | "while" "(" expr ")" stmt              -> while_stmt
       | "def" NAME "(" parlist? ")" body         -> func_def_stmt
       | block

  ?block: "{" stmt* "}"         -> block

  ?body: "{" stmt* "return" expr "}"        -> body

  ?range: "[" INT ".." INT "]"        -> range_block

  ?parlist: NAME ("," NAME)*            -> param_list

  ?expr: "lambda" NAME ":" expr         -> lambda_expr
       | coper 

  ?explist: expr ("," expr)*        -> args

  ?coper: aoper "<" aoper           -> ltop
        | aoper ">" aoper           -> gtop
        | aoper "<=" aoper          -> leop
        | aoper ">=" aoper          -> geop
        | aoper "==" aoper          -> eqop
        | aoper "!=" aoper          -> neop
        | aoper

  ?aoper: aoper "+" moper       -> add
        |  aoper "-" moper       -> sub
        |  moper         

  ?moper: moper "*" unary       -> mul
        | moper "/" unary        -> div
        | unary

  ?unary: "-" atom             -> neg
        | "!" atom             -> notop
        | preop

  ?preop: "++" atom
        | "--" atom
        | atom

  ?atom: "(" expr ")"
       | NAME           -> name
       | (INT | FLOAT)  -> num

  COMMENT: /:[^\n]*/

  %import common.CNAME      -> NAME
  %import common.INT        -> INT
  %import common.FLOAT      -> FLOAT
  %import common.WS

  %ignore WS
  %ignore COMMENT
"""

parser = Lark(grammar, parser='lalr')

# Variable environment
#
class Env(dict):
    prev = []
    # scope management
    def openScope(self):
        self.prev.insert(0, self)
        return Env()

    # close scope
    def closeScope(self):
        return self.prev.pop(0)

    # variable operations
    def extend(self,x,v):
        if x in self:
            raise Exception("Variable Already Exists")
        else:
            self[x] = v

    # look up variable
    def lookup(self,x):
        if x in self:
            return self[x]
        else:
            for env in self.prev:
                if x in env:
                    return env[x]
        raise Exception(f"Undefined Variable '{x}'")

    # update variable
    def update(self,x,v):
        if x in self:
            self[x] = v
            return
        else:
            for env in self.prev:
                if x in env:
                    env[x] = v
                    return
        raise Exception("Undefined Variable")

env = Env()

# Closure
#
class Closure():
    # function closure
    def __init__(self,ids,body,env):
        self.ids = ids
        self.body = body
        self.env = env

# Interpreter
#
@v_args(inline=True)
class Eval(Interpreter):
    # represents an integer
    def num(self, val): return int(val)

    # converts a number to negative
    def neg(self, x): return -self.visit(x)

    # represents a name of an object
    def name(self, x): 
        global env
        return env.lookup(x)

    # arithmetic operations
    def div(self,x,y): return self.visit(x) // self.visit(y)
    def mul(self,x,y): return self.visit(x) * self.visit(y)
    def sub(self,x,y): return self.visit(x) - self.visit(y)
    def add(self,x,y): return self.visit(x) + self.visit(y)

    # function call
    def func_call_stmt(self,f,args):
        global env
        temp_env = env
        c = self.visit(f)
        env = c.env.openScope()
        args = self.visit(args)
        if len(args) != len(c.ids):
            raise Exception("Parameter Error")
        for i in range(len(args)):
            env.extend(c.ids[i], args[i])
        val = self.visit(c.body)
        env = env.closeScope()
        env = temp_env
        return val

    # function definition
    def lambda_expr(self,x,e):
        return Closure(x, e, copy.deepcopy(env))

    # statements
    def block(self, *s):
        global env
        env = env.openScope()
        for stmt in s:
            self.visit(stmt)
        env = env.closeScope()

    # print statement
    def print_stmt(self,e):
        print(self.visit(e))

    # variable declaration
    def var_decl_stmt(self,x,e):
        global env
        env.extend(x, self.visit(e))

    # function declaration
    def func_def_stmt(self, f, x, body):
        global env
        x = self.visit(x)
        c = Closure(x, body, copy.deepcopy(env))
        env.extend(f, c)
        c.env.extend(f, c)

    # function body
    def body(self, *s):
        expr = s[-1]
        for stmt in s[:-1]:
            self.visit(stmt)
        return self.visit(expr)

    # variable assignment
    def assign_stmt(self, x, e):
        global env
        env.update(x, self.visit(e))

    # if statement
    def if_stmt(self, e, s1, s2):
        if self.visit(e) != 0:
            self.visit(s1)
        elif s2:
            self.visit(2)

    # while statement
    def while_stmt(self, e, s):
        while self.visit(e) != 0:
            self.visit(s)

    # expression list
    def args(self,*x):
        args = []
        for a in x:
            args.append(self.visit(a))
        return args

    # parameter list
    def param_list(self,*x):
        args = []
        for a in x:
            args.append(a)
        return args
    
    # for statement
    def for_stmt(self, id, r, s):
        rng = self.visit(r)
        if rng[0] > rng[1]:
            steps = -1
        else:
            steps = 1
        # for loop's own scope
        global env
        env = env.openScope()
        env.extend(id, 0)
        for i in range(rng[0], rng[1], steps):
            env.update(id, i)
            self.visit(s)
        env = env.closeScope()
    
    # range block
    def range_block(self, start, end):
        return (int(start), int(end))
    
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


# Main function
#
import sys
def main():
    try:
        prog = sys.stdin.read()
        tree = parser.parse(prog)
        print(prog, end="")
        Eval().visit(tree)
    except Exception as e:
        print(e)

if __name__ == "__main__":
    main()

