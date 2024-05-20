import re

from .value import Cons, Symbol, INERT

__all__ = ["Parser"]

class Token:
    def __init__(self, type, value, line, col, pos):
        self.type = type
        self.value = value
        self.line = line
        self.col = col
        self.pos = pos
    
    def __repr__(self):
        return f"Token({self.type!r}, {self.value[0]!r})"

class Tokenizer:
    def __init__(self, s):
        self.s = s
        self.line = 1
        self.col = 1
        self.pos = 0
    
    def run(self):
        tr = re.compile(r"""\s*(?:
            (?P<num>
                \d+\.\d*(?:[eE][-+]\d+)|
                \d+[eE][-+]\d+|
                \.\d+(?:[eE][-+]\d+)?
            )|
            (?P<int>\d+)|
            (?P<punc>[()\[\]{},.'`~^@]|\#;)|
            (?P<sym>[^\s:\[\](){},;"'`~^@]+)|
            (?P<key>:[^\s:\[\](){},"'`~^@]+)|
            "(?P<str>(?:[\\].|[^\\"])*)"|
            (?P<com>;.*)|
            (?P<err>.)
        )""", re.X|re.M)
        
        for t in tr.finditer(self.s):
            lns = t[0].splitlines()
            if len(lns) > 1:
                self.line += len(lns) - 1
                self.col = len(lns[-1])
            else:
                self.col += len(lns[0])
            self.pos = t.end()
            
            if m := t['err']:
                raise SyntaxError(f"Unexpected character: {m!r}")
            elif t['com']:
                continue
            elif m := t['punc']:
                yield Token(m, t, self.line, self.col, self.pos)
            else:
                yield Token(t.lastgroup, t, self.line, self.col, self.pos)

class Parser:
    def __init__(self, code):
        self.tokenizer = Tokenizer(code)
        self.tokens = self.tokenizer.run()
        self.cur = None
    
    def eof(self):
        if self.cur is None:
            return self.tokenizer.pos >= len(self.tokenizer.s)
        else:
            return False
    
    def peek(self):
        if self.cur is None:
            self.cur = next(self.tokens)
        return self.cur
    
    def consume(self):
        '''Advance the iterator, returning the most recent token.'''
        tok = self.peek()
        self.cur = None
        return tok
    
    def maybe(self, tt):
        '''Consume the next token if it matches the given type, otherwise return None.'''
        try:
            return self.consume() if self.peek().type == tt else None
        except StopIteration:
            return None
    
    def expect(self, tt):
        '''Consume the next token if it matches the given type, otherwise raise an error.'''
        if tok := self.maybe(tt):
            return tok
        raise SyntaxError(f"Expected {tt}, got {self.peek().type}")
    
    def read_list(self):
        ls = []
        while not self.maybe(")"):
            if self.maybe('.'):
                if len(ls) == 0:
                    raise SyntaxError("Improper list must have at least one element")
                
                expr = self.read()
                if expr is INERT:
                    raise SyntaxError("No object for improper list cdr")
                c = Cons.from_list(ls, expr)
                self.expect(')')
                return c
            else:
                expr = self.read()
                if expr is INERT:
                    self.expect(")")
                    break
                ls.append(expr)
        
        return Cons.from_list(ls)
    
    def read_atom(self):
        if tok := self.maybe('('):
            expr = self.read_list()
            if isinstance(expr, Cons):
                expr.plist = {"pos": (tok.line, tok.col)}
            return expr
        elif tok := self.maybe("'"):
            return Cons(Symbol("quote"), Cons(self.read()))
        
        try:
            if tok := self.maybe('int'):
                return int(tok.value['int'])
            elif tok := self.maybe('sym'):
                return Symbol(tok.value['sym'])
            elif tok := self.maybe('str'):
                return tok.value['str']
        except StopIteration:
            pass
        return INERT
    
    def read(self):
        while True:
            # Ignore next datum
            comment = self.maybe("#;")
            expr = self.read_atom()
            if not comment:
                return expr