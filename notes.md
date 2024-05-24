class Opcode(Enum):
OP_NOP = 0
OP_LOOP = auto()
OP_BLOCK = auto()
OP_BR = auto()
OP_NIL = auto()
OP_CONST = auto()
OP_LIST = auto()
OP_MOVE = auto()
OP_LDUP = auto()
OP_STUP = auto()
OP_CONS = auto()
OP_CAR = auto()
OP_CDR = auto()
OP_CLOSE = auto()
OP_RET = auto()
OP_CALL = auto()
OP_TAIL = auto()
OP_JUMP = auto()
OP_SEQ = auto()
OP_SLT = auto()
OP_SLE = auto()
OP_ADD = auto()
OP_SUB = auto()
OP_MUL = auto()
OP_DIV = auto()
OP_IDIV = auto()
OP_MOD = auto()
OP_REM = auto()
OP_CAT = auto()

Racket's Qi library looks very powerful
https://web.cs.wpi.edu/~jshutt/dissertation/etd-090110-124904-Shutt-Dissertation.pdf
https://github.com/Robert-van-Engelen/tinylisp/blob/main/src/tinylisp-commented.c
https://pengowray.github.io/wasm-ops/
https://try.scheme.org/
https://github.com/vito/pumice/ - vau calculus reference implementation
https://github.com/JeffBezanson/femtolisp/blob/master/tiny/lisp.c - high-performance lisp interpreter

The original Python interpreter took over 10 seconds just to run the prelude because of how many layers of abstraction I implemented in lisp, so I reimplemented in C. This led to a debuggability problem, though; I no longer had the ability to assign arbitrary properties like parsing metadata. To get around this, I came up with the concept of "meta-cons", which is a specially annotated cons which passes operations like car/cdr to its signified value (car) but has additional operations which allow direct access to the metadata (cdr).