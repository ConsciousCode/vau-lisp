#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <stdbool.h>
#include <setjmp.h>
#include <stdint.h>

#include <readline/readline.h>
#include <readline/history.h>
#include <signal.h>

/// Constants ///
#define MAX_ATOMS 65536
#define MAX_CELLS (65536/sizeof(Cell)/2)
#define TYPE_BITS 3
#define ORD_BITS (sizeof(Value)*8 - TYPE_BITS)
#define ORD_MASK ~(((1<<TYPE_BITS) - 1) << (sizeof(Value)*8 - TYPE_BITS))
#define BUFFER_SIZE 40

#define box_(type, ordinal) (type << (sizeof(Value)*8 - TYPE_BITS) | (ordinal&ORD_MASK))

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

typedef uint32_t Value;
typedef struct {
    Value car, cdr;
} Cell;

typedef struct {
    const char* name;
    Value (*fn)(Value, Value);
} BuiltinEntry;

BuiltinEntry get_builtin(int ord);

/// Global variables ///
#define INIT_ATOMS(F) \
    F(UNBOUND, "") F(QUOTE, "quote") F(VAU, "$vau") \
    F(WRAP, "wrap") F(UNWRAP, "unwrap") F(META, "meta") \
    F(TRUE, "#t") F(INERT, "#inert") F(IGNORE, "#ignore") \
    F(RECENT, "#")

#define INIT_ATOMS_ENUM(name, str) Value SYM_ ## name;
INIT_ATOMS(INIT_ATOMS_ENUM)

static volatile int int_state = 0;
const char *errmsg = 0;
const Value nil = box_(T_CONS, 0), unbound = box_(T_SYMBOL, 0);
char atoms[MAX_ATOMS] = {0};
Cell a_cells[MAX_CELLS] = {0}, b_cells[MAX_CELLS] = {0};
Cell *fromspace = a_cells, *tospace = b_cells, *cells = a_cells;
size_t hp = 0, sp = 0; // Heap and stack pointer
sigjmp_buf toplevel;

_Noreturn void error(const char *msg, ...) {
    if(msg) {
        va_list args;
        va_start(args, msg);
        fprintf(stderr, "Error ");
        vfprintf(stderr, msg, args);
        fprintf(stderr, "\n");
    }
    int_state = 0;
    longjmp(toplevel, 1);
}

ValueType type(Value v) {
    return v >> ORD_BITS;
}
int ordinal(Value v) {
    return ((int32_t)(v << TYPE_BITS)) >> TYPE_BITS;
}
bool iscons(Value v) { return v && type(v) == T_CONS; }
bool isconslike(Value v) { return iscons(v) || type(v) == T_CAPSULE; }
Value box(ValueType type, int ord) {
    if(ordinal(ord) != ord) error("box: ordinal %d too large", ord);
    return box_(type, ord);
}

typedef struct {
    void (*write)(const char *, void *);
    void *data;
} Writer;
Writer lisp_stdout;

__attribute__((__format__(__printf__, 2, 3)))
void write_fmt(Writer *w, const char *fmt, ...) {
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

typedef struct {
    char *buf;
    size_t len;
    size_t pos;
} string_writer_data;
void string_write_cb(const char *s, string_writer_data *buf) {
    size_t len = strnlen(s, buf->len);
    if(buf->pos + len + 1 >= buf->len) {
        error("string_write_cb: buffer full");
    }
    memcpy(buf->buf + buf->pos, s, len + 1);
    buf->pos += len;
}
void string_writer(Writer *w, string_writer_data *buf) {
    w->write = (void (*)(const char *, void *))&string_write_cb;
    w->data = buf;
}
char *tostring(Value v);

const char *typeof_string(Value v) {
    if(v == nil) return "nil";
    if(v == unbound) return "unbound";
    switch(type(v)) {
        case T_INT: return "int";
        case T_SYMBOL: return "symbol";
        case T_CONS: return "cons";
        case T_CAPSULE: return "capsule";
        case T_BUILTIN: return "builtin";
        default: return "unknown";
    }
}

Value intern(const char *sym) {
    //printf("Intern %s\n", sym);
    size_t i = 0;
    while(i < hp) {
        if(strcmp(&atoms[i], sym) == 0) {
            return box(T_SYMBOL, i);
        }
        i += strlen(&atoms[i]) + 1;
    }
    size_t len = strlen(sym) + 1;
    hp += len;
    if(hp >= MAX_ATOMS) error("out of memory: symbol %s", sym);
    
    memcpy(&atoms[i], sym, len);
    return box(T_SYMBOL, i);
}
Value cons(Value car, Value cdr) {
    cells[++sp] = (Cell){car, cdr};
    if(sp >= MAX_CELLS) error("out of memory: cons %u", sp);
    return box(T_CONS, sp);
}
Value car(Value v) {
    if(iscons(v)) return car_(v);
    error("car: not a cons");
}
Value cdr(Value v) {
    if(iscons(v)) return cdr_(v);
    error("cdr: not a cons");
}

void write_looped(Writer *w, Value v, Value *seen);

int check_seen(Value v, Value *seen) {
    if(!isconslike(v)) return 0;
    int i;
    for(i = 0; i < 1024 && seen[i]; ++i) {
        if(seen[i] == v) return i + 1;
    }
    if(i == 1024) error("write_looped: seen too many");
    seen[i] = v;
    return 0;
}

void write_ls(Writer *w, Value v, Value *seen);
void write_capsule(Writer *w, Value v, Value *seen) {
    write_fmt(w, "#(");
    if(car_(v) == SYM_VAU) {
        write_fmt(w, "$vau");
        if(isconslike(cdr_(v))) {
            write_fmt(w, " #E ");
            write_ls(w, cdr_(cdr_(v)), seen);
        }
    }
    else {
        write_ls(w, v, seen);
    }
    write_fmt(w, ")#");
}

void write_ls(Writer *w, Value v, Value *seen) {
    for(;;) {
        write_looped(w, car_(v), seen);
        v = cdr_(v);
        // Problem: Need to run check_seen to see if the cdr is a loop
        // But then, if we break later and v is not nil, write_looped will
        // already have v in seen, so it will print a number
        int i = check_seen(v, seen);
        if(i) {
            if(iscons(seen[i])) {
                write_fmt(w, " . #%d#", i);
            }
            else {
                Value sym = car_(v);
                const char *name =
                    type(sym) == T_SYMBOL? &atoms[ordinal(sym)] : typeof_string(sym);
                write_fmt(w, " . #(%d %s)#", i, name);
            }
            return;
        }
        if(iscons(v)) {
            write_fmt(w, " ");
            continue;
        }
        if(isconslike(v)) {
            write_fmt(w, " . ");
            write_capsule(w, v, seen);
            return;
        }
        break;
    }
    if(v) {
        write_fmt(w, " . ");
        write_looped(w, v, seen);
    }
}

void write_looped(Writer *w, Value v, Value *seen) {
    if(v == nil) {
        write_fmt(w, "()");
        return;
    }
    
    int i = check_seen(v, seen);
    if(i) {
        if(iscons(seen[i])) {
            write_fmt(w, "!#%d#", i);
        }
        else {
            Value sym = car_(v);
            const char *name =
                type(sym) == T_SYMBOL? &atoms[ordinal(sym)] : typeof_string(sym);
            write_fmt(w, "!#(%d %s)#", i, name);
        }
        return;
    }
    
    switch(type(v)) {
        case T_INT:
            write_fmt(w, "%d", ordinal(v));
            break;
        case T_SYMBOL:
            if(v == SYM_UNBOUND) {
                write_fmt(w, "<unbound>");
            }
            else {
                write_fmt(w, "%s", &atoms[ordinal(v)]);
            }
            break;
        case T_CONS:
            write_fmt(w, "(");
            write_ls(w, v, seen);
            write_fmt(w, ")");
            break;
        case T_CAPSULE:
            write_capsule(w, v, seen);
            break;
        case T_BUILTIN:
            write_fmt(w, "<builtin %s>", get_builtin(ordinal(v)).name);
            break;
    }
}

void write_ob(Writer *w, Value v) {
    Value seen[1024] = {0};
    write_looped(w, v, seen);
}

void print(Value v) {
    write_ob(&lisp_stdout, v);
}

char *tostring(Value v) {
    static char buf[4096];
    string_writer_data bl = {buf, sizeof(buf), 0};
    Writer w;
    string_writer(&w, &bl);
    write_ob(&w, v);
    return buf;
}

bool iscap(Value tag, Value value) {
    return type(value) == T_CAPSULE && car_(value) == tag;
}
Value encap(Value tag, Value value) {
    return box(T_CAPSULE, ordinal(cons(tag, value)));
}
Value decap(Value tag, Value value) {
    if(iscap(tag, value)) return cdr_(value);
    error("decap: tag mismatch");
}

// (vau env param penv . body)
Value vau_env(Value v) {
    if(iscap(SYM_VAU, v)) return car(cdr_(v));
    error("vau_env: not a vau");
}
Value vau_param(Value v) {
    if(iscap(SYM_VAU, v)) return car(cdr(cdr_(v)));
    error("vau_param: not a vau");
}
Value vau_penv(Value v) {
    if(iscap(SYM_VAU, v)) return car(cdr(cdr(cdr_(v))));
    error("vau_penv: not a vau");
}
Value vau_body(Value v) {
    if(iscap(SYM_VAU, v)) return cdr(cdr(cdr(cdr_(v))));
    error("vau_body: not a vau");
}

// (wrap appv)
Value wrap(Value v) { return encap(SYM_WRAP, v); }
Value unwrap(Value v) { return decap(SYM_WRAP, v); }

// (meta v meta)
Value get_meta(Value v) { return iscap(SYM_META, v)? cdr(cdr_(v)) : nil; }
Value get_unmeta(Value v) { return iscap(SYM_META, v)? car(cdr_(v)) : v; }

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
    Value frame = car(env);
    Value *lu = lookup(sym, frame);
    if(lu) {
        *lu = val;
    }
    else {
        car_(env) = cons(cons(sym, val), frame);
    }
}
Value push_scope (Value env) {
    return cons(nil, env);
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
            lu = scoped_lookup(v, env);
            if(lu) return *lu;
            error("eval: unbound symbol \"%s\"", &atoms[ordinal(v)]);
        
        // Application
        case T_CONS: {
            if(v == nil) return nil;
            Value args = cdr_(v);
            for(;;) {
                v = get_unmeta(car_(v));
                if(v == nil) error("eval apply: nil");
                Value op = eval(v, env);
                switch(type(op)) {
                    case T_INT: error("eval apply: int");
                    case T_SYMBOL: error("eval apply: symbol");
                    case T_CONS: error(op? "eval apply: cons" : "eval apply: nil");
                    case T_BUILTIN:
                        printf("Apply builtin %s ", get_builtin(ordinal(op)).name);
                        printf("with %s\n", tostring(args));
                        return get_builtin(ordinal(op)).fn(args, env);
                    case T_CAPSULE:
                        if(iscap(SYM_WRAP, op)) {
                            op = unwrap(op);
                            // Evaluate arguments
                            Value eargs = nil, cur = nil;
                            while(iscons(args)) {
                                v = cons(eval(car_(args), env), nil);
                                if(eargs) { // Append
                                    cdr_(cur) = v;
                                }
                                else { // Assign
                                    eargs = v;
                                }
                                cur = v;
                                args = cdr_(args);
                            }
                            // Improper list
                            if(args) cdr_(cur) = eval(args, env);
                            printf("Apply %s ", tostring(op));
                            printf("with %s\n", tostring(eargs));
                            
                            return eval(cons(op, eargs), env);
                        }
                        else if(iscap(SYM_VAU, op)) {
                            env = push_scope(env);
                            Value param = vau_param(op);
                            Value penv = vau_penv(op);
                            if(param != SYM_IGNORE) define(param, args, env);
                            if(penv != SYM_IGNORE) define(penv, env, env);
                            
                            printf("Apply vau %s ", tostring(op));
                            printf("with %s\n", tostring(args));
                            
                            Value body = vau_body(op);
                            Value last = nil;
                            while(iscons(body)) {
                                printf("Evaluating %s\n", tostring(car_(body)));
                                last = eval(car_(body), env);
                                body = cdr_(body);
                            }
                            return last;
                        }
                        else {
                            error("eval apply: unknown capsule");
                        }
                }
            }
            error("eval cons: ???");
        }
    }
    error("eval: ???");
}

// (vau param penv . body)
Value bi_vau(Value args, Value env) {
    Value
        param = car(args),
        penv = car(cdr(args)),
        body = cdr(cdr(args));
    
    return encap(SYM_VAU, cons(env, cons(param, cons(penv, body))));
}

Value bi_eval(Value args, Value env) {
    if(type(env) != T_CONS) error("eval: bad env");
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
    define(car(args), eval(car(cdr(args)), env), env);
    return SYM_INERT;
}

Value bi_print(Value args, UNUSED Value env) {
    if(args) {
        if(!iscons(args)) error("print: not a list");
        
        print(car(args));
        args = cdr(args);
        do {
            putchar(' ');
            print(car(args));
            args = cdr(args);
        } while(iscons(args));
    }
    putchar('\n');
    return SYM_INERT;
}

Value bi_error(Value args, Value env) {
    bi_print(args, env);
    error(NULL);
}

Value bi_select(Value args, UNUSED Value env) {
    if(type(args) != T_CONS) error("select: no args");
    Value c = car(args);
    Value t = car(cdr(args));
    Value f = car(cdr(cdr(args)));
    return c? t : f;
}

Value bi_eq(Value args, UNUSED Value env) {
    return ordinal(car(args)) == ordinal(car(cdr(args)))? SYM_TRUE : nil;
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
#define DEF_ID 6
BuiltinEntry builtins[] = {
    {"$vau", bi_vau},
    {"eval", bi_eval},
    {"car", bi_car},
    {"cdr", bi_cdr},
    {"wrap", bi_wrap},
    {"unwrap", bi_unwrap},
    {"$def!", bi_define},
    {"print", bi_print},
    {"error", bi_error},
    {"select", bi_select},
    {"eq?", bi_eq},
    {"+", bi_plus},
    {"-", bi_minus},
    {"*", bi_mul},
    {"/", bi_div},
    {"%", bi_mod}
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

typedef struct {
    char *buf;
    size_t len;
    size_t pos;
} string_reader_data;

int string_read_cb(string_reader_data *data) {
    return data->pos < data->len? data->buf[data->pos++] : EOF;
}
void string_reader(Reader *r, string_reader_data *buf) {
    r->read = (int(*)(void *))&string_read_cb;
    r->data = buf;
    r->nextc = 0;
}

char consume(Reader *r) {
    if(r->nextc == EOF) return EOF;
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

void read_space(Reader *r) {
    while(peek(r) <= ' ' && peek(r) != EOF) consume(r);
}

Value parse(Reader *r);

bool isstop(char c) {
    return c <= ' ' || c == '(' || c == ')' || c == '\'' || c == ';';
}

char scan(Reader *r) {
    read_space(r);
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
            } while(i < BUFFER_SIZE && !isstop(peek(r)));
            if(i == BUFFER_SIZE) {
                r->buf[i - 1] = '\0';
                error("read: token too long \"%s...\"", r->buf);
            }
            break;
    }
    r->buf[i] = '\0';
    return r->buf[0];
}

Value parse(Reader *r) {
    Value v, cur;
    int n, i;
    switch(r->buf[0]) {
        case '(':
            v = nil;
            cur = nil;
            while(scan(r) != ')') {
                if(strcmp(r->buf, ".") == 0) {
                    Value x = parse(r);
                    scan(r);
                    return cons(v, x);
                }
                Value x = cons(parse(r), nil);
                if(v) {
                    cdr_(cur) = x;
                }
                else {
                    v = x;
                }
                cur = x;
            }
            return v;
        case ')': error("read: unexpected ')'");
        case '\'': scan(r); return cons(SYM_QUOTE, cons(parse(r), nil));
        case '"':
            i = strlen(r->buf) - 1;
            if(r->buf[i] == '"') {
                r->buf[i] = '\0';
                return intern(&r->buf[1]);
            }
            error("read: unterminated string");
        default:
            return sscanf(r->buf, "%d%n", &n, &i) > 0 && r->buf[i] == '\0'?
                box(T_INT, n) : intern(r->buf);
    }
}

Value read_ob(Reader *r) {
    char c = scan(r);
    return (c && c != EOF)? parse(r) : SYM_INERT;
}

Value relocate(Value v) {
    if(v == nil) return v;
    //printf("Relocate %d %s\n", v, tostring(v));
    switch(type(v)) {
        case T_INT:
        case T_SYMBOL:
        case T_BUILTIN:
            return v;
        case T_CONS:
        case T_CAPSULE: {
            Cell cell = fromspace[ordinal(v)];
            if(cell.car == unbound) return cell.cdr;
            cells = fromspace;
            //printf("Relocate %d %d: ", type(v), ordinal(v));
            //print(v);
            //printf("\n");
            cells = tospace;
            Value c = box(type(v), ++sp);
            fromspace[ordinal(v)] = (Cell) { unbound, c };
            tospace[ordinal(c)] = (Cell) {
                relocate(cell.car),
                relocate(cell.cdr)
            };
            //printf("Relocated %d %d: ", type(v), ordinal(v));
            //print(c);
            //printf("\n");
            return c;
        }
    }
    error("relocate: ???");
}

Value gc(Value env) {
    printf("GC %lu ->", sp);
    sp = 0;
    cells = tospace;
    Value e = relocate(env);
    tospace = fromspace;
    fromspace = cells;
    printf(" %lu\n", sp);
    return e;
}

void int_handler(UNUSED int sig) {
    ++int_state;
    siglongjmp(toplevel, 1);
}

/* Lisp initialization and REPL */
int main() {
    struct sigaction act = {.sa_handler = int_handler};
    sigaction(SIGINT, &act, NULL);
    
    rl_readline_name = "vau-lisp";
    rl_attempted_completion_function = NULL;
    
    #define INIT_ATOMS_INTERN(name, str) SYM_ ## name = intern(str);
    INIT_ATOMS(INIT_ATOMS_INTERN)
    
    Value env = cons(nil, nil);
    for(size_t i = 0; i < sizeof(builtins)/sizeof(builtins[0]); ++i) {
        Value v = box(T_BUILTIN, i);
        if(i != VAU_ID && i != DEF_ID) v = wrap(v);
        define(intern(builtins[i].name), v, env);
    }
    Reader r;
    string_reader_data read_data;
    string_reader(&r, &read_data);
    file_writer(&lisp_stdout, stdout);
    
    // ^C run (-1) -> none (0) -> once (1) -> quit
    // Error * -> none
    // In the REPL, run <-> none
    if(sigsetjmp(toplevel, 1)) {
        if(int_state == 1) {
            printf("Press Ctrl-C again to quit\n");
            goto repl;
        }
        else if(int_state > 1) {
            putchar('\n');
            return 0;
        }
    }
    
    repl:
    for(;;) {
        char *line = readline("\x01\x1b[7m\x02ϝ>>\x01\x1b[0m\x02 ");
        if(line == NULL) break; // CTRL-D EOF
        
        if(line[0] == '/') {
            if(strcmp(&line[1], "quit") == 0) break;
            else if(strcmp(&line[1], "gc") == 0) env = gc(env);
            else if(strcmp(&line[1], "env") == 0) {
                print(env);
                printf("\n");
            }
            else {
                printf("Unknown command: %s\n", line);
                continue;
            }
            add_history(line);
            continue;
        }
        
        // Empty line
        int i;
        for(i = 0; line[i]; ++i) {
            if(!isspace(line[i])) break;
        }
        if(line[i] == '\0') continue;
        
        read_data = (string_reader_data){line, strlen(line), 0};
        string_reader(&r, &read_data);
        Value v = read_ob(&r);
        int_state = -1; // run
        printf("Read %s\n", tostring(v));
        v = eval(v, env);
        // Only add to history if it's not an error
        add_history(line);
        if(v == SYM_INERT) {
            continue;
        }
        define(SYM_RECENT, v, env);
        print(v);
        putchar('\n');
        
        //print(eval(read(&r), env));
        env = gc(env);
        int_state = 0; // none
    }
    return 0;
}