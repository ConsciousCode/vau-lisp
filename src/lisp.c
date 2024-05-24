#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <setjmp.h>

/// Constants ///
#define MAX_ATOMS 65536
#define MAX_CELLS (65536/sizeof(Cell)/2)
#define TYPE_BITS 3
#define ORD_BITS (sizeof(Value)*8 - TYPE_BITS)
#define ORD_MASK ~(((1<<TYPE_BITS) - 1) << (sizeof(Value)*8 - TYPE_BITS))
#define BUFFER_SIZE 40

#define car_(v) cells[ordinal(v)].car
#define cdr_(v) cells[ordinal(v)].cdr

#define UNUSED __attribute__((unused))

/// Types ///
typedef enum {
    T_CONS = 0,
    T_SYMBOL,
    T_INT,
    T_CAPSULE,
    T_BUILTIN
} ValueType;

typedef int32_t Value;

typedef struct {
    Value car, cdr;
} Cell;

typedef struct {
    const char* name;
    Value (*fn)(Value, Value);
} BuiltinEntry;

BuiltinEntry get_builtin(int ord);

ValueType type(Value v) {
    return v >> ORD_BITS;
}
int ordinal(Value v) {
    return (v & ORD_MASK) | ((v & (1<<ORD_BITS))? ~ORD_MASK : 0);
}

/// Global variables ///
const char *errmsg = 0;
const Value nil = 0, unbound = T_SYMBOL;
Value sym_inert, sym_ignore, sym_quote, sym_vau, sym_wrap, sym_unwrap, sym_meta, sym_tru;
char atoms[MAX_ATOMS];
Cell a_cells[MAX_CELLS], b_cells[MAX_CELLS];
Cell *fromspace = a_cells, *tospace = b_cells, *cells = a_cells;
size_t hp = 0, sp = 0; // Heap and stack pointer
jmp_buf toplevel;

_Noreturn void error(const char *msg, ...) {
    if(msg) {
        va_list args;
        va_start(args, msg);
        vfprintf(stderr, msg, args);
    }
    longjmp(toplevel, 1);
}

/**
 * Value type is represented using cons.
**/
bool iscons(Value v) {
    return v && type(v) == T_CONS;
}

Value box(ValueType type, int ordinal) {
    return type << (sizeof(Value)*8 - TYPE_BITS) | ordinal;
}
Value intern(const char *sym) {
    size_t i = 0;
    while(i < hp) {
        if(strcmp(&atoms[i], sym) == 0) {
            return box(T_SYMBOL, i);
        }
        i += strlen(&atoms[i]) + 1;
    }
    hp += strlen(strcpy(&atoms[i], sym)) + 1;
    if(hp >= sp*sizeof(Cell)) error("out of memory: symbol\n");
    
    return box(T_SYMBOL, i);
}
Value cons(Value car, Value cdr) {
    cells[--sp] = (Cell){car, cdr};
    if(hp >= sp*sizeof(Cell)) error("out of memory: cons\n");
    return box(T_CONS, sp);
}

Value car(Value v) {
    if(type(v) != T_CONS) error("car: not a cons\n");
    return car_(v);
}
Value cdr(Value v) {
    if(type(v) != T_CONS) error("cdr: not a cons\n");
    return cdr_(v);
}

Value captag(Value v) {
    if(type(v) == T_CAPSULE) return car_(v);
    error("captop: not a capsule\n");
}
bool iscap(Value tag, Value value) { return captag(value) == tag; }
Value encap(Value tag, Value value) {
    return box(T_CAPSULE, ordinal(cons(tag, value)));
}
Value decap(Value tag, Value value) {
    if(iscap(tag, value)) return cdr_(value);
    error("decap: tag mismatch\n");
}

// (vau env param penv . body)
Value vau_env(Value v) {
    if(iscap(sym_vau, v)) return car(cdr_(v));
    error("vau_env: not a vau\n");
}
Value vau_param(Value v) {
    if(iscap(sym_vau, v)) return car(cdr(cdr_(v)));
    error("vau_param: not a vau\n");
}
Value vau_penv(Value v) {
    if(iscap(sym_vau, v)) return car(cdr(cdr(cdr_(v))));
    error("vau_penv: not a vau\n");
}
Value vau_body(Value v) {
    if(iscap(sym_vau, v)) return cdr(cdr(cdr(cdr_(v))));
    error("vau_body: not a vau\n");
}

// (wrap appv)
Value wrap(Value v) { return encap(sym_wrap, v); }
Value unwrap(Value v) { return decap(sym_wrap, v); }

// (meta v meta)
Value get_meta(Value v) { return iscap(sym_meta, v)? cdr(cdr_(v)) : nil; }
Value get_unmeta(Value v) { return iscap(sym_meta, v)? car(cdr_(v)) : v; }

Value *lookup(Value sym, Value frame) {
    while(frame) {
        Value binding = car(frame);
        if(car(binding) == sym) return &cdr_(binding);
        frame = cdr(frame);
    }
    return NULL;
}
Value *scoped_lookup(Value sym, Value env) {
    while(env) {
        Value *result = lookup(sym, car(env));
        if(result) return result;
        env = cdr(env);
    }
    return NULL;
}
void define(Value sym, Value val, Value env) {
    car_(env) = cons(cons(sym, val), car_(env));
}
Value push_scope (Value env) {
    return cons(nil, env);
}

Value eval(Value v, Value env);

Value list(Value tail, Value env) {
    if(type(tail) != T_CONS) return eval(tail, env);
    return cons(eval(car(tail), env), list(cdr(tail), env));
}

Value eval(Value v, Value env) {
    Value *lu;
    v = get_unmeta(v);
    switch(type(v)) {
        // Self-evaluating
        case T_INT:
        case T_BUILTIN:
        case T_CAPSULE: return v;
        
        // Symbol lookup
        case T_SYMBOL:
            lu = lookup(v, env);
            if(lu) return *lu;
            error("eval: unbound symbol\n");
        
        // Application
        case T_CONS: {
            Value args = cdr(v);
            for(;;) {
                Value op = get_unmeta(car(v));
                switch(type(op)) {
                    case T_INT: error("eval apply: int\n");
                    case T_SYMBOL: error("eval apply: symbol\n");
                    case T_CONS: error(op? "eval apply: cons" : "eval apply: nil");
                    case T_BUILTIN: return get_builtin(ordinal(op)).fn(args, env);
                    case T_CAPSULE:
                        if(iscap(sym_wrap, op)) {
                            v = unwrap(op);
                            args = list(args, env);
                            return eval(v, env);
                        }
                        else if(iscap(sym_vau, op)) {
                            env = push_scope(env);
                            Value param = vau_param(op);
                            if(param != sym_ignore) {
                                define(param, args, env);
                            }
                            Value penv = vau_penv(op);
                            if(penv != sym_ignore) {
                                define(penv, env, env);
                            }
                            
                            Value body = vau_body(op);
                            Value last = nil;
                            while(iscons(body)) {
                                last = eval(car_(body), env);
                            }
                            return last;
                        }
                        else {
                            error("eval apply: unknown capsule\n");
                        }
                }
            }
            error("eval cons: ???\n");
        }
    }
    error("eval: ???\n");
}

typedef struct {
    void (*write)(const char *, void *);
    void *data;
} Writer;

void write(Writer *w, const char * fmt, ...) {
    va_list args;
    va_start(args, fmt);
    char buf[256];
    vsnprintf(buf, sizeof(buf), fmt, args);
    w->write(buf, w->data);
}

void file_write_cb(const char *s, void *f) {
    fprintf((FILE *)f, "%s", s);
}
void file_writer(Writer *w, FILE *f) {
    w->write = (void (*)(const char *, void *))&file_write_cb;
    w->data = f;
}

void print(Writer *w, Value v) {
    switch(type(v)) {
        case T_INT: write(w, "%d", ordinal(v)); break;
        case T_SYMBOL: write(w, "%s", atoms + ordinal(v)); break;
        case T_CONS: {
            write(w, "(");
            print(w, car(v));
            v = cdr(v);
            while(type(v) == T_CONS) {
                write(w, " ");
                print(w, car(v));
                v = cdr(v);
            }
            if(v) {
                write(w, " . ");
                print(w, v);
            }
            write(w, ")");
            break;
        }
        case T_CAPSULE:
            write(w, "#(");
            print(w, car(v));
            write(w, " ");
            print(w, cdr(v));
            write(w, ")#");
            break;
        case T_BUILTIN:
            write(w, "<builtin %s>", get_builtin(ordinal(v)).name);
            break;
    }
}

// (vau param penv . body)
Value bi_vau(Value args, Value env) {
    Value
        param = car(args),
        penv = car(cdr(args)),
        body = cdr(cdr(args));
    
    return encap(sym_vau, cons(env, cons(param, cons(penv, body))));
}

Value bi_eval(Value args, Value env) {
    if(type(env) != T_CONS) error("eval: bad env\n");
    return eval(car(args), env);
}

Value bi_car(Value args, UNUSED Value env) {
    return car(car(args));
}

Value bi_cdr(Value args, UNUSED Value env) {
    return cdr(car(args));
}

Value bi_wrap(Value args, UNUSED Value env) {
    return wrap(car(args));
}

Value bi_unwrap(Value args, UNUSED Value env) {
    return unwrap(car(args));
}

Value bi_define(Value args, Value env) {
    define(car(args), car(cdr(args)), env);
    return sym_inert;
}

Value bi_print(Value args, UNUSED Value env) {
    Writer out;
    file_writer(&out, stdout);
    while(iscons(args)) {
        print(&out, car(args));
        args = cdr(args);
    }
    return sym_inert;
}

Value bi_error(Value args, Value env) {
    bi_print(args, env);
    error(NULL);
}

Value bi_select(Value args, UNUSED Value env) {
    if(type(args) != T_CONS) error("select: no args\n");
    Value c = car(args);
    Value t = car(cdr(args));
    Value f = car(cdr(cdr(args)));
    return c? t : f;
}

Value bi_eq(Value args, UNUSED Value env) {
    return ordinal(car(args)) == ordinal(car(cdr(args)))? sym_tru : nil;
}

bool op_args(Value args, Value *lhs, Value *rhs) {
    *lhs = car(args);
    *rhs = car(cdr(args));
    return !(type(*lhs) == T_INT && type(*rhs) == T_INT);
}
#define SIMPLE_OP(name, op) \
    Value bi_ ## name(Value args, UNUSED Value env) { \
        Value lhs, rhs; \
        if(op_args(args, &lhs, &rhs)) error(#name ": not int"); \
        return box(T_INT, ordinal(lhs) op ordinal(rhs)); \
    }

SIMPLE_OP(plus, +)
SIMPLE_OP(minus, -)
SIMPLE_OP(mul, *)
SIMPLE_OP(div, /)
SIMPLE_OP(mod, %)

#define VAU_ID 0
BuiltinEntry builtins[] = {
    {"$vau", bi_vau},
    {"def!", bi_define},
    {"eval", bi_eval},
    {"car", bi_car},
    {"cdr", bi_cdr},
    {"wrap", bi_wrap},
    {"unwrap", bi_unwrap},
    {"print", bi_print},
    {"error", bi_error}
};

BuiltinEntry get_builtin(int ord) {
    return builtins[ord];
}

typedef struct {
    int (*read)(void *);
    void *data;
    char buf[BUFFER_SIZE];
    char nextc;
} Reader;

char consume(Reader *r) {
    if(r->nextc) {
        char c = r->nextc;
        r->nextc = 0;
        return c;
    }
    return r->read(r->data);
}

char peek(Reader *r) {
    return r->nextc? r->nextc : (r->nextc = consume(r));
}

Value parse(Reader *r);

char scan(Reader *r) {
    while(peek(r) <= ' ') {
        consume(r);
    }
    int i = 0;
    char c;
    switch(peek(r)) {
        case '(': case ')':
        case '\'':
            r->buf[i++] = consume(r);
            break;
        
        case ';':
            do {
                c = consume(r);
            } while(c != '\n' && c != EOF);
            return scan(r);
        
        case EOF: return EOF;
        
        // Datum comment or symbol?
        case '#':
            r->buf[i++] = consume(r);
            if(peek(r) == ';') {
                consume(r);
                scan(r);
                parse(r);
                return scan(r);
            }
            [[fallthrough]];
        default:
            do {
                r->buf[i++] = consume(r);
                c = peek(r);
            } while(i < BUFFER_SIZE && !(c == '(' || c == ')' || c <= ' '));
            break;
    }
    r->buf[i] = 0;
    return r->buf[0];
}

Value parse(Reader *r) {
    Value v;
    int n, i;
    switch(r->buf[0]) {
        case '(':
            v = nil;
            while(scan(r) != ')') {
                if(strcmp(r->buf, ".") == 0) {
                    Value x = parse(r);
                    scan(r);
                    return cons(v, x);
                }
                Value x = cons(parse(r), nil);
                if(v) cells[ordinal(v)].cdr = x;
                v = x;
            }
            return v;
        case ')': error("read: unexpected ')'\n");
        case '\'': return cons(sym_quote, cons(parse(r), nil));
        case '"':
            i = strlen(r->buf) - 1;
            if(r->buf[i] == '"') {
                r->buf[i] = 0;
                return intern(r->buf + 1);
            }
            else {
                error("read: unterminated string\n");
            }
        default:
            return sscanf(r->buf, "%d%n", &n, &i) > 0 && r->buf[i] == '\0'?
                box(T_INT, n) : intern(r->buf);
    }
}

Value read(Reader *r) {
    scan(r);
    return r->buf[0]? parse(r) : nil;
}

Value relocate(Value v) {
    if(!iscons(v)) return v;
    if(car(v) == unbound) return cdr(v);
    
    Value nc = cons(relocate(car(v)), relocate(cdr(v)));
    cells[ordinal(v)] = (Cell) {unbound, nc};
    return nc;
}

Value gc(Value env) {
    cells = tospace;
    // lim = curheap+heapsize-sizeof(cons_t);
    env = relocate(env);
    
    Cell *tmp = tospace;
    tospace = fromspace;
    fromspace = tmp;
    
    return env;
}

/* Lisp initialization and REPL */
int main() {
    if(setjmp(toplevel)) {
        printf("Symbol table overflow.\n");
        return 1;
    }
    
    intern(""); // unbound symbol
    sym_inert = intern("#inert");
    sym_ignore = intern("#ignore");
    sym_quote = intern("quote");
    sym_vau = intern("$vau");
    sym_wrap = intern("wrap");
    sym_unwrap = intern("unwrap");
    sym_meta = intern("meta");
    sym_tru = intern("#t");
    
    Value env = cons(nil, nil);
    for(size_t i = 0; i < sizeof(builtins)/sizeof(builtins[0]); ++i) {
        Value v = box(T_BUILTIN, i);
        if(i != VAU_ID) v = wrap(v);
        define(intern(builtins[i].name), v, env);
    }
    Reader r = { .read = (int(*)(void *))&fgetc, .data = stdin };
    Writer w;
    file_writer(&w, stdout);
    
    if(setjmp(toplevel)) {
        printf("longjmp received.\n");
        env = gc(env);
    }
    
    for(;;) {
        printf("\x01\x1b[7m\x02ϝ>>\x01\x1b[0m\x02 ");
        if(peek(&r) == '/') {
            consume(&r);
            scan(&r);
            if(strcmp(r.buf, "quit") == 0) break;
            else if(strcmp(r.buf, "gc") == 0) env = gc(env);
            else if(strcmp(r.buf, "env") == 0) {
                print(&w, env);
                printf("\n");
            }
            else {
                printf("Unknown command: /%s\n", r.buf);
            }
            continue;
        }
        Value v = eval(read(&r), env);
        if(v == sym_inert) {
            continue;
        }
        
        print(&w, eval(read(&r), env));
        env = gc(env);
    }
    return 0;
}