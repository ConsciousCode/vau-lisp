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
        
        ri = read_iter(s)
        try:
            for expr in ri:
                eval(expr, env)
        except Exception as e:
            e.add_note(f"Failed before line {ri.line}")
            raise
    
    repl(Environment({}, env))

if __name__ == '__main__':
    main()