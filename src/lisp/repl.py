import readline
import operator
import traceback as tb
from collections import UserDict
from functools import reduce, wraps

from .parser import Parser, INERT
from .value import Cons, Environment, Symbol, Operative, Applicative, Builtin, display

def read_iter(s):
    p = Parser(s)
    try:
        while True:
            expr = p.read()
            if expr is INERT:
                if p.eof():
                    break
                raise SyntaxError(f"Unexpected character: {p.peek().value[0]}")
            yield expr
    except StopIteration:
        raise SyntaxError(f"Unexpected EOF before line {p.tokenizer.line}")# from None
    except (SyntaxError, GeneratorExit):
        raise
    except:
        tb.print_exc()
        print("Failed before line", p.tokenizer.line)
        raise

def read(s):
    for expr in read_iter(s):
        return expr

STRATEGY = False

def eval(expr, env):
    match expr:
        # Symbol lookup
        case Symbol(name):
            return env[name]
        
        case Operative(): return expr
        
        # Catch-all
        
        case Cons(car, cdr):
            match eval(car, env):
                #case Cons():
                #    return eval(Cons(eval(car, env), cdr), env)
                
                case Operative(senv, ptree, penv, body):
                    #print("Eval operative", car, body, senv)
                    lenv = Environment({}, senv)
                    if not lenv.define(ptree, cdr):
                        raise TypeError(f"Couldn't match ptree {display(ptree)} with {display(cdr)}")
                    if not lenv.define(penv, env):
                        raise TypeError(f"Couldn't match penv {display(penv)} with {display(env)}")
                    
                    last = None
                    if STRATEGY:
                        while True:
                            match body:
                                case None: break
                                
                                case Cons(ar, dr):
                                    print("Cons", ar, dr)
                                    last = eval(ar, lenv)
                                    body = dr
                                
                                case _:
                                    print("Operative Non-cons cdr", body)
                                    body = eval(body, lenv)
                                    #print("Result:", body)
                                    break
                                    if not isinstance(cdr, Cons):
                                        break
                    else:
                        if isinstance(body, Cons):
                            for expr in body:
                                last = eval(expr, lenv)
                        else:
                            body = eval(body, lenv)
                        
                    return last
                
                case Applicative(combiner):
                    if cdr is None:
                        args = None
                    else:
                        ls = []
                        if STRATEGY:
                            while True:
                                match cdr:
                                    case None:
                                        print("None")
                                        break
                                    
                                    case Cons(ar, dr):
                                        print("Cons", ar, dr)
                                        ls.append(eval(ar, env))
                                        cdr = dr
                                    
                                    case _:
                                        print("Applicative Non-cons cdr", cdr)
                                        while cdr is not None:
                                            while isinstance(cdr, Cons):
                                                cdr = cdr.tail
                                            
                                            cdr = eval(cdr, env)
                                            if cdr is None:
                                                break
                                        print("Cdr result:", cdr)
                                        if not isinstance(cdr, Cons):
                                            print("Not cons:", cdr)
                                            break
                            args = Cons.from_list(ls, cdr)
                        else:
                            if isinstance(cdr, Cons):
                                for cdr in cdr.pairs():
                                    ls.append(eval(cdr.head, env))
                                args = Cons.from_list(ls, eval(cdr.tail, env))
                            else:
                                #print("Not cons:", cdr)
                                args = eval(cdr, env)
                                #print("evaluated cdr:", args)
                    
                    if isinstance(combiner, Cons):
                        raise TypeError(f"Combiner is a cons: {combiner}")
                    
                    #return combiner(env, args)
                    return eval(Cons(combiner, args), env)
                
                case Builtin(fn):
                    return fn(env, cdr)
                
                case na:
                    raise TypeError(f"Applying a non-combiner: {type(na)} {na} {cdr}")
        
        case _:
            return expr

class REPL:
    HISTORY = "./.espresso_history"
    PROMPT = "\x01\x1b[7m\x02ϝ>>\x01\x1b[0m\x02 " #🡢
    
    def __init__(self, env):
        self.env = env
        self.hlen = 0
    
    def complete(self, text, state):
        m = [k for k in self.env.keys() if k.startswith(text)]
        try:
            return m[state]
        except IndexError:
            return None
    
    def register(self):
        readline.set_history_length(1000)
        readline.set_completer(self.complete)
        readline.set_completer_delims(" ()[];")
        readline.parse_and_bind("tab: complete")
    
    def start(self):
        readline.set_startup_hook(self.register)
        
        try:
            readline.read_history_file(self.HISTORY)
            self.hlen = readline.get_current_history_length()
        except FileNotFoundError:
            open(self.HISTORY, 'wb').close()
            self.hlen = 0
    
    def input(self):
        expr = input(self.PROMPT)
        readline.add_history(expr)
        nhlen = readline.get_current_history_length()
        readline.append_history_file(nhlen - self.hlen, self.HISTORY)
        self.hlen = nhlen
        
        return expr

def repl(env=None):
    if env is None:
        env = Environment(ENV)
        
    ctrl = REPL(env)
    ctrl.start()
    
    try:
        while True:
            try:
                expr = ctrl.input()
                
                if expr == "":
                    pass
                elif expr[0] == "/":
                    match expr[1:]:
                        case "exit": break
                        case "env": print(env)
                        case _: print(f"Unknown command {expr}")
                else:
                    parsed = read(expr)
                    result = eval(parsed, env)
                    if result is INERT:
                        print()
                    else:
                        env["#"] = result
                        print(display(result))
            except (EOFError, StopIteration):
                break
            except Exception as e:
                tb.print_exc()
    except KeyboardInterrupt:
        pass
    print()

def cond(e, *cases):
    for case in cases:
        test = case.head
        body = case.tail
        if test == Symbol("else") or eval(test, e):
            return eval(body, e)
    raise ValueError("No true condition in $cond")

CONSTS = {
    "#ignore": Symbol("#ignore")
}

def error(*args):
    raise ValueError(" ".join(map(str, args)))

def make_env(p=None):
    return Environment({}, p)

APPLICATIVES = {
    "+": lambda *args: sum(args),
    "-": lambda *args: -args[0] if len(args) == 1 else reduce(operator.sub, args),
    "*": lambda *args: reduce(operator.mul, args, 1),
    "/": lambda *args: 1/args[0] if len(args) == 1 else reduce(operator.truediv, args),
    "%": lambda *args: reduce(operator.mod, args),
    "==": lambda *args: all(x == args[0] for x in args),
    "!=": lambda *args: any(x != args[0] for x in args),
    "<": lambda *args: all(x < args[0] for x in args),
    ">": lambda *args: all(x > args[0] for x in args),
    "<=": lambda *args: all(x <= args[0] for x in args),
    ">=": lambda *args: all(x >= args[0] for x in args),
    "car": lambda x: x.head,
    "cdr": lambda x: x.tail,
    "error": error,
    "cons": Cons,
    "wrap": Applicative,
    "unwrap": Applicative.unwrap,
    "length": len,
    "eval": eval,
    "symbol": Symbol,
    "null?": lambda x: x is None,
    "cons?": lambda x: isinstance(x, Cons),
    "symbol?": lambda x: isinstance(x, Symbol),
    "environment?": lambda x: isinstance(x, Environment),
    "operative?": lambda x: isinstance(x, Operative),
    "applicative?": lambda x: isinstance(x, Applicative),
    "combiner?": lambda x: isinstance(x, (Operative, Applicative)),
    "builtin?": lambda x: isinstance(x, Builtin),
    "bool?" : lambda x: isinstance(x, bool),
    "int?": lambda x: isinstance(x, int),
    "str?": lambda x: isinstance(x, str),
    "eq?" : lambda x, y: x is y or x == y,
    "display": display,
    "make-environment": make_env,
    "print": print
}

def define(env, name, value):
    env.define(name, eval(value, env))
    return INERT

OPERATIVES = {
    "$def!": define,
    "$if": lambda e, c, t, f: eval(t if eval(c, e) else f, e)
}

def e_dotp(k, v):
    @wraps(v)
    def wrapper(e, p):
        return v(e, *Cons.to_iter(p))
    return Builtin(k, wrapper)

def dotp(k, v):
    @wraps(v)
    def wrapper(_, p):
        return v(*Cons.to_iter(p))
    return Applicative(Builtin(k, wrapper))

COMBINERS = {
    **{k: e_dotp(k, v) for k, v in OPERATIVES.items()},
    **{k: dotp(k, v) for k, v in APPLICATIVES.items()},
    #"list": Applicative(Builtin("list", builtin_list)),
    "$vau": Builtin("$vau", Operative)
}

class BuiltinEnv(UserDict):
    def __repr__(self):
        return "<builtins>"

# Sample environment with addition function
ENV = BuiltinEnv({
    **CONSTS,
    **COMBINERS
})