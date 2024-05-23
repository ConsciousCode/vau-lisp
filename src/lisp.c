#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>

#define MEM_SIZE 65536
#define TYPE_BITS 3
#define ORD_BITS (sizeof(Value)*8 - TYPE_BITS)
#define ORD_MASK ~(((1<<TYPE_BITS) - 1) << (sizeof(Value)*8 - TYPE_BITS))
#define BUFFER_SIZE 40

typedef enum {
    T_CONS = 0,
    T_INT,
    T_SYMBOL,
    T_BUILTIN,
    T_CLOSURE,
    T_META // Metadata
} ValueType;

typedef int32_t Value;

typedef struct {
    const char* name;
    Value (*fn)(Value, Value);
} BuiltinEntry;

BuiltinEntry get_builtin(int ord);

ValueType type(Value v) {
    return v >> ORD_BITS;
}

int ordinal(Value v) {
    Value x = v & ORD_MASK;
    // Sign extend
    return (v & (1<<ORD_BITS))? x | ~ORD_MASK : x;
}

const char *errmsg = 0;
const Value nil = 0;
Value sym_inert, sym_ignore, sym_err, sym_quote, sym_name, sym_line;

Value memory[MEM_SIZE]; // Shared memory for cells and atoms
int hp = 0, sp = MEM_SIZE; // Heap and stack pointers

_Noreturn void panic(const char *msg, ...) {
    va_list args;
    va_start(args, msg);
    vfprintf(stderr, msg, args);
    exit(1);
}

Value error(const char *msg) {
    errmsg = msg;
    return sym_err;
}

Value get_unmeta(Value v) {
    return type(v) == T_META ? memory[ordinal(v)] : v;
}
Value get_meta(Value v) {
    return type(v) == T_META ? memory[ordinal(v) + 1] : nil;
}

Value box(ValueType type, int ordinal) {
    return type << (sizeof(Value)*8 - TYPE_BITS) | ordinal;
}
Value intern(const char *sym) {
    int i = 0;
    char *it = (char *)memory;
    while(i < hp) {
        if(strcmp((char *)memory + i, sym) == 0) {
            return box(T_SYMBOL, i);
        }
        it += strlen(it) + 1;
    }
    hp += strlen(strcpy(it, sym)) + 1;
    if(hp > sp*sizeof(Value)) return error("out of memory: symbol");
    
    return box(T_SYMBOL, i);
}
Value cons(Value car, Value cdr) {
    memory[--sp] = cdr;
    memory[--sp] = car;
    if(hp > sp*sizeof(Value)) return error("out of memory: cons");
    return box(T_CONS, sp);
}

Value car(Value v) {
    switch(type(v)) {
        case T_INT: return error("car: int");
        case T_CLOSURE: return error("car: closure");
        case T_SYMBOL: return box(T_INT, ordinal(v));
        case T_BUILTIN: return intern(get_builtin(ordinal(v)).name);
        case T_CONS: return memory[ordinal(v)];
        case T_META: return car(get_unmeta(v));
    }
    panic("car: ???");
}
Value cdr(Value v) {
    switch(type(v)) {
        case T_INT: return error("cdr: int");
        case T_CLOSURE: return error("cdr: closure");
        case T_SYMBOL:
            return ((char *)memory)[ordinal(v) + 1]?
                box(T_SYMBOL, ordinal(v) + 1) : nil;
        case T_BUILTIN: return nil;
        case T_CONS: return memory[ordinal(v) + 1];
        case T_META: return cdr(get_unmeta(v));
    }
    panic("cdr: ???");
}

Value clos_op(Value v) { return cdr(car(v)); }
Value clos_env(Value v) { return cdr(v); }
Value clos_args(Value v) { return car(v); }

Value lookup(Value sym, Value tab) {
    while(tab) {
        Value binding = car(tab);
        if(car(binding) == sym) {
            return cdr(binding);
        }
        tab = cdr(tab);
    }
    return sym_err;
}

Value scoped_lookup(Value sym, Value env) {
    if(type(sym) != T_SYMBOL) panic("lookup: not a symbol");
    
    while(env) {
        Value result = lookup(sym, car(env));
        if(result == sym_err) {
            return result;
        }
        env = cdr(env);
    }
    return error("unbound symbol");
}

Value eval(Value v, Value env);

Value list(Value tail, Value env) {
    if(type(tail) != T_CONS) return eval(tail, env);
    return cons(eval(car(tail), env), list(cdr(tail), env));
}

Value bind(Value sym, Value val, Value env) {
    switch(type(sym)) {
        case T_CONS: return bind(
            car(sym), car(val),
            bind(cdr(sym), cdr(val), env)
        );
        case T_SYMBOL: return cons(cons(sym, val), env);
        case T_INT: return error("bind: int");
        case T_BUILTIN: return error("bind: builtin");
        case T_CLOSURE: return error("bind: closure");
        case T_META: return bind(memory[])
    }
    panic("bind: ???");
}

Value eval_raw(Value v, Value env) {
    switch(type(v)) {
        // Self-evaluating
        case T_INT:
        case T_BUILTIN:
        case T_CLOSURE: return v;
        case T_META: return eval_raw(get_unmeta(v), env);
        
        // Symbol lookup
        case T_SYMBOL: return lookup(v, env);
        
        // Application
        case T_CONS: {
            Value args = cdr(v);
            for(;;) {
                Value op = get_unmeta(car(v));
                switch(type(op)) {
                    case T_INT: { // Int returns the nth argument
                        int i = ordinal(op);
                        while(i--) args = cdr(args);
                        return car(args);
                    }
                    case T_SYMBOL: return error("eval apply: symbol");
                    case T_CONS: return error(op? "eval apply: cons" : "eval apply: nil");
                    case T_BUILTIN: return get_builtin(ordinal(op)).fn(args, env);
                    case T_CLOSURE: return eval(
                        clos_op(op),
                        bind(
                            clos_args(op),
                            list(args, env),
                            clos_env(op)? env : cdr(op)
                        )
                    );
                    case T_META: continue;
                }
            }
            panic("eval cons: ???");
        }
    }
    panic("eval: ???");
}

void fprint_value(FILE* stream, Value v) {
    switch(type(v)) {
        case T_INT: fprintf(stream, "%d", ordinal(v)); break;
        case T_SYMBOL: fprintf(stream, "%s", (char *)memory + ordinal(v)); break;
        case T_CONS: {
            fprintf(stream, "(");
            fprint_value(stream, car(v));
            v = cdr(v);
            while(type(v) == T_CONS) {
                fprintf(stream, " ");
                fprint_value(stream, car(v));
                v = cdr(v);
            }
            if(v) {
                fprintf(stream, " . ");
                fprint_value(stream, v);
            }
            fprintf(stream, ")");
            break;
        }
        case T_META: {
            fprintf(stream, "#(meta ");
            fprint_value(stream, car(v));
            fprintf(stream, " ");
            fprint_value(stream, cdr(v));
            fprintf(stream, ")#");
            break;
        }
        case T_BUILTIN: fprintf(stream, "<builtin %s>", get_builtin(ordinal(v)).name); break;
        case T_CLOSURE: fprintf(stream, "<closure>"); break;
    }
}

Value eval(Value v, Value env) {
    Value result = eval_raw(v, env);
    if(errmsg && type(v) == T_CONS) {
        Value meta = get_meta(v);
        if(meta) {
            Value name = lookup(sym_name, meta);
            Value line = lookup(sym_line, meta);
            if(type(name) == T_SYMBOL && type(line) == T_INT) {
                fprintf(stderr, "%s @ %d\n",
                    (char *)memory + ordinal(name), ordinal(line));
            }
        }
    }
    return result;
}

Value push_pair(Value name, Value value, Value env) {
    return cons(cons(name, value), env);
}

Value push_scope (Value env) {
    return cons(nil, env);
}

Value bi_vau(Value args, Value env) {
    if(type(args) != T_CONS) return error("vau: no args");
    Value op = car(args);
    if(type(op) != T_CONS) return error("vau: no op");
    Value clos = cons(op, cons(cdr(args), env));
    return cons(box(T_CLOSURE, ordinal(clos)), clos);
}

Value bi_eval(Value args, Value env) {
    if(type(args) != T_CONS) return error("eval: no args");
    return eval(car(args), env);
}

Value bi_car(Value args, Value env) {
    if(type(args) != T_CONS) return error("car: no args");
    return car(car(args));
}

Value bi_cdr(Value args, Value env) {
    if(type(args) != T_CONS) return error("cdr: no args");
    return cdr(car(args));
}

Value bi_wrap(Value args, Value env) {
    if(type(args) != T_CONS) return error("wrap: no args");
    return cons(box(T_META, ordinal(car(args))), car(args));
}

Value bi_unwrap(Value args, Value env) {
    if(type(args) != T_CONS) return error("unwrap: no args");
    return get_unmeta(car(args));
}

Value bi_define(Value args, Value env) {
    if(type(args) != T_CONS) return error("define: no args");
    Value sym = car(args);
    if(type(sym) != T_SYMBOL) return error("define: not a symbol");
    Value val = eval(car(cdr(args)), env);
    if(val == sym_err) return val;
    return bind(sym, val, env);
}

Value bi_print(Value args, Value env) {
    if(type(args) != T_CONS) return error("print: no args");
    fprint_value(stdout, car(args));
    return nil;
}

Value bi_error(Value args, Value env) {
    if(type(args) != T_CONS) return error("error: no args");
    return error((char *)memory + ordinal(car(args)));
}

BuiltinEntry builtins[] = {
    {"def!", bi_define},
    {"$vau", bi_vau},
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
                if(v) {
                    memory[ordinal(v) + 1] = x;
                }
                v = x;
            }
            return v;
        case ')': return error("read: unexpected ')'");
        case '\'': return cons(sym_quote, cons(parse(r), nil));
        case '"':
            i = strlen(r->buf) - 1;
            if(r->buf[i] == '"') {
                r->buf[i] = 0;
                return intern(r->buf + 1);
            }
            else {
                return error("read: unterminated string");
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

/* Lisp initialization and REPL */
int main() {
    intern("");
    sym_inert = intern("#inert");
    sym_ignore = intern("#ignore");
    sym_err = intern("#ERR");
    sym_quote = intern("quote");
    sym_name = intern("name");
    sym_line = intern("line");
    
    Value env = cons(nil, nil);
    for(int i = 0; i < sizeof(builtins)/sizeof(builtins[0]); ++i) {
        env = push_pair(
            intern(builtins[i].name),
            box(T_BUILTIN, i),
            env
        );
    }
    Reader r = { .read = (int(*)(void *))&fgetc, .data = stdin };
    
    for(;;) {
        printf("\x01\x1b[7m\x02ϝ>>\x01\x1b[0m\x02 ");
        Value v = eval(read(&r));
        if(v == sym_inert) {
            continue;
        }
        
        fprint_value(stdout, eval(read(&r), env));
        sp = ordinal(env); // "gc"?
    }
}