import readline
import operator
import traceback as tb
from collections import UserDict
from functools import reduce, wraps
import inspect

from .parser import Parser, INERT
from .value import Cons, Environment, Symbol, Operative, Applicative, Builtin, display

class read_iter:
    def __init__(self, s):
        self.parser = Parser(s)
    
    def __iter__(self):
        try:
            while True:
                expr = self.parser.read()
                if expr is INERT:
                    if self.parser.eof():
                        break
                    raise SyntaxError(f"Unexpected character: {self.parser.peek().value[0]}")
                yield expr
        except StopIteration:
            raise SyntaxError(f"Unexpected EOF before line {self.parser.tokenizer.line}")# from None
        except:
            raise
    
    @property
    def line(self):
        return self.parser.tokenizer.line

def read(s):
    for expr in read_iter(s):
        return expr

vau = Builtin("$vau", Operative)

def e_dotp(k, v, inc_e=True):
    sig = inspect.signature(v)
    count = 0 - inc_e
    for x in sig.parameters.values():
        if x.kind == inspect.Parameter.VAR_POSITIONAL:
            break
        count += 1
    else:
        @wraps(v)
        def wrap_posonly(e, p):
            args = Cons.to_list(p)
            if len(args) != count:
                raise TypeError(f"{k!r} expected {count} arguments, got {len(args)}: {args}")
            return v(e, *args)
        return Builtin(k, wrap_posonly)
        
    @wraps(v)
    def wrap_varargs(e, p):
        return v(e, *Cons.to_iter(p))
    return Builtin(k, wrap_varargs)

def dotp(k, v):
    @wraps(v)
    def forward(e, *args):
        return v(*args)
    return Applicative(e_dotp(k, forward, False))

define = e_dotp("def!", Environment.define)

def eval(expr, env):
    if not isinstance(env, Environment):
        raise TypeError(f"Environment expected, got {type(env).__name__}: {env}")
    
    match expr:
        # Symbol lookup
        case Symbol(name):
            return env[name]
        
        case Operative(): return expr
        
        # Catch-all
        
        case Cons(car, cdr):
            if env.line is None:
                env.line = expr.line
            
            oper = eval(car, env)
            try:
                match oper:
                    #case Cons():
                    #    return eval(Cons(eval(car, env), cdr), env)
                    
                    case Operative(senv, ptree, penv, body) as op:
                        #print("\x1b[7mEval operative\x1b[m", op, car, ptree, body, senv)
                        lenv = Environment({}, senv)
                        if isinstance(expr, Cons):
                            lenv.line = expr.line
                        
                        if not lenv.defvar(ptree.name, cdr):
                            raise TypeError(f"Couldn't match ptree {display(ptree)} with {display(cdr)}")
                        if not lenv.defvar(penv.name, env):
                            raise TypeError(f"Couldn't match penv {display(penv)} with {display(env)}")
                        
                        last = None
                        if not isinstance(body, Cons):
                            #print("Non-cons body", body)
                            body = eval(body, lenv)
                        
                        for expr in Cons.to_iter(body):
                            #print("Eval", expr)
                            last = eval(expr, lenv)
                            
                        return last
                    
                    case Applicative(combiner):
                        #print("Applying", combiner, cdr)
                        if cdr is None:
                            args = None
                        else:
                            ls = []
                            if isinstance(cdr, Cons):
                                for cdr in cdr.pairs():
                                    ls.append(eval(cdr.head, env))
                                args = Cons.from_list(ls, eval(cdr.tail, env))
                            else:
                                #print("Not cons:", cdr)
                                args = eval(cdr, env)
                                #print("evaluated cdr:", args)
                            
                            if args is not None:
                                args.line = expr.line
                        
                        if isinstance(combiner, Cons):
                            raise TypeError(f"Combiner is a cons: {combiner}")
                        
                        return eval(Cons(combiner, args), env)
                    
                    case Builtin(fn) as bi:
                        result = fn(env, cdr)
                        return result
                    
                    case na:
                        raise TypeError(f"Applying a non-combiner: {car} == {type(na).__name__} {na} to {cdr}")
                        
            except Exception as e:
                if line := getattr(expr, 'line', None):
                    e.add_note(f"{getattr(car, 'name', '(anonymous)')} @ {line}")
                raise
        
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
                        case "#": print(display(env['#']))
                        case "exit": break
                        case "env": print(env)
                        case "builtins":
                            b = None
                            e = env
                            while e is not None:
                                if isinstance(e.head, BuiltinEnv):
                                    b = e
                                    break
                                e = e.tail
                            else:
                                print("No builtins found")
                                continue
                            print(dict(e.head))
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

CONSTS = {
    "#ignore": Symbol("#ignore")
}

def error(*args):
    raise ValueError(" ".join(map(str, args)))

def make_env(p=None):
    return Environment({}, p)

def head(x):
    if isinstance(x, Cons):
        return x.head
    raise TypeError(f"Can't get head of {x}")

def tail(e, x):
    if isinstance(x, Cons):
        t = x.tail
        if isinstance(t, Cons):
            return t
        return eval(t, e)
    
    raise TypeError(f"Tail of non-cons {type(x).__name__} {x}")

def typeof(x):
    match x:
        case None: return None
        case int(): return "int"
        case str(): return "str"
        case bool(): return "bool"
        case Symbol(): return "symbol"
        case Environment(): return "environment"
        case Cons(): return "cons"
        case Operative(): return "operative"
        case Applicative(): return "applicative"
        case Builtin(): return "builtin"
        case _: return f"unknown:{type(x).__name__}"

def safe_getattr(x, k):
    try:
        return getattr(x, k)
    except AttributeError:
        return None

def safe_setattr(x, k, v):
    try:
        setattr(x, k, v)
    except AttributeError:
        print(f"Setting {type(x).__name__} {x} failed", k, v)
    return x

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
    "head": head,
    "select": lambda c, t, f: t if c else f,
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
    "print": print,
    "typeof": typeof,
    "setattr!": safe_setattr,
    "getattr": safe_getattr,
}

OPERATIVES = {
}

COMBINERS = {
    **{k: e_dotp(k, v) for k, v in OPERATIVES.items()},
    **{k: dotp(k, v) for k, v in APPLICATIVES.items()},
    #"list": Applicative(Builtin("list", builtin_list)),
    "$vau": vau,
    "def!": Applicative(define)
}

class BuiltinEnv(UserDict):
    def __repr__(self):
        return "<builtins>"

# Sample environment with addition function
ENV = BuiltinEnv({
    **CONSTS,
    **COMBINERS
})