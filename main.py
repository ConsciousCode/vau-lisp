#!/usr/bin/env python3

import sys
from pathlib import Path
sys.path.append(f"{Path(__file__).parents[0]}/src")

from lisp import repl, read_iter, eval, ENV, Environment

def main():
    env = Environment(ENV)
    for fn in ["prelude.lisp"]:
        with open(f"./stdlib/{fn}") as f:
            s = f.read()
        
        for expr in read_iter(s):
            eval(expr, env)
    
    repl(Environment({}, env))

if __name__ == '__main__':
    main()