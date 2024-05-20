from typing import Mapping, Any, Optional

__all__ = (
    "display",
    "Cons",
    "Symbol",
    "Environment",
    "Operative",
    "Applicative",
    "Builtin",
    "INERT"
)

class Inert:
    def __init__(self):
        raise NotImplementedError("Inert is a singleton")
    
    def __repr__(self):
        return "#inert"
INERT = Inert.__new__(Inert)

def display(v):
    match v:
        #case Symbol(n): return n
        case None: return "()"
        case True: return "true"
        case False: return "false"
        case str(s):
            s = s.replace("\\", "\\\\").replace('"', '\\"')
            return f'"{s}"'
    
    #return repr(type(v).__name__)
    return repr(v)

class Cons:
    __match_args__ = ("head", "tail")
    
    def __init__(self, head, tail=None, plist=None):
        self.head = head
        self.tail = tail
        self.plist = {} if plist is None else plist
    
    def __repr__(self) -> str:
        s = []
        cur = self
        while isinstance(cur, Cons):
            s.append(display(cur.head))
            cur = cur.tail
            if cur is None:
                break
        else:
            s.append(f". {cur}")
        
        base = f"({' '.join(s)})"
        #'''
        if self.plist:
            if pos := self.plist.get('pos'):
                ln, _ = pos
                return f"{base}@{ln}"
        #'''
        return base
    
    def __len__(self):
        L = 0
        for _ in self.pairs():
            L += 1
        return L
    
    def pairs(self):
        cur = self
        while isinstance(cur, Cons):
            yield cur
            cur = cur.tail
    
    def __iter__(self):
        for p in self.pairs():
            yield p.head
    
    def __getitem__(self, x):
        if x < 0:
            x += len(self)
        
        cur = self
        for _ in range(x):
            cur = cur.tail
            if not isinstance(cur, Cons):
                raise IndexError(x)
        return cur.head
    
    def __eq__(self, other):
        if not isinstance(other, Cons): return False
        if len(self) != len(other): return False
        
        for a, b in zip(self.pairs(), other.pairs()):
            if a.head != b.head:
                return False
        
        return a.tail == b.tail # type: ignore
    
    @staticmethod
    def from_list(ls, cdr=None):
        L = len(ls)
        if L == 0:
            return cdr
        
        it = iter(ls)
        head = Cons(next(it))
        cur = head
        for x in it:
            new = Cons(x)
            cur.tail = new
            cur = new
        
        cur.tail = cdr
        return head
    
    @staticmethod
    def to_iter(cons):
        if cons is not None:
            yield from cons
    
    @staticmethod
    def to_list(cons):
        return list(Cons.to_iter(cons))

class Symbol:
    __match_args__ = ("name",)
    
    interns = {}
    
    def __new__(cls, name):
        if name in cls.interns:
            return cls.interns[name]
        self = super().__new__(cls)
        cls.interns[name] = self
        return self
    
    def __init__(self, name):
        self.name = name
    
    def __repr__(self):
        return f"{self.name}"
    
    def __eq__(self, other):
        return isinstance(other, Symbol) and self.name == other.name

MISSING = object()
class Environment(Cons):
    def __init__(self, env:Mapping[str, Any]|None=None, parent:Optional['Environment']=None):
        if env is None:
            env = {}
        super().__init__(env, parent)
    
    def __getitem__(self, key: int|str|Symbol):
        if isinstance(key, int):
            return super().__getitem__(key)
        
        if isinstance(key, Symbol):
            key = key.name
        
        for p in self.pairs():
            val = p.head.get(key, MISSING)
            if val is not MISSING:
                return val
        
        raise KeyError(key)
    
    def __setitem__(self, key, value):
        self.head[key] = value
    
    def __repr__(self) -> str:
        s = []
        cur = self
        while isinstance(cur, Cons):
            s.append(display(cur.head))
            cur = cur.tail
            if cur is None:
                break
        else:
            s.append(f". {cur}")
        
        return f"Environment({' ~ '.join(s)})"
    
    def define(self, param, value):
        '''Optimized for lists but supports arbitrary trees and improper lists'''
        match param:
            case str(name):
                self[name] = value
                if isinstance(value, Applicative):
                    value = value.combiner
                if isinstance(value, Operative):
                    value.name = name
                return INERT
            case _:
                raise TypeError(f"def! must take a symbol: {type(param).__name__}")

class Operative:
    __match_args__ = ("env", "ptree", "penv", "body")
    
    name: Optional[str]
    env: Environment
    ptree: Symbol
    penv: Symbol
    body: Cons
    
    def __init__(self, env, args):
        self.name = None
        self.env = env
        self.ptree = args.head
        self.penv = args.tail.head
        self.body = args.tail.tail
        
        if isinstance(self.body, Cons):
            if len(self.body) == 0:
                if self.body.tail is None:
                    raise TypeError("Operative body must have at least one expression")
    
    def __repr__(self):
        return f"<$vau {self.ptree} {self.penv} {self.body}>"
        if self.name is None:
            return "<$vau (anonymous)>"
        return f"<$vau {self.name}>"

class Applicative:
    '''Wrapper for applicatives so (unwrap (wrap appv)) == appv in all cases'''
    __match_args__ = ("combiner",)
    
    def __init__(self, combiner):
        self.combiner = combiner
    
    def __repr__(self):
        n = 0
        cur = self.combiner
        while isinstance(cur, Applicative):
            n += 1
            cur = cur.combiner
        c = f":{n}" if n > 0 else ""
        return f"<applicative{c} {display(self.combiner)}>"
    
    @staticmethod
    def unwrap(appv):
        if isinstance(appv, Applicative):
            return appv.combiner
        raise TypeError(f"Expected applicative, got {appv}")

class Builtin:
    __match_args__ = ("fn",)
    
    def __init__(self, name, fn):
        self.name = name
        self.fn = fn
    
    def __call__(self, env, ptree):
        return self.fn(env, ptree)
    
    def __repr__(self):
        return f"<builtin {self.name}>"