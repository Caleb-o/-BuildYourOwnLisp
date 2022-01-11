#define DEBUG

#include <stdio.h>
#include <assert.h>
#include <string.h>
#ifdef DEBUG
#include <time.h>
#endif

#include "mpc/mpc.h"


#define REPL_BUF 2048
#define ERR_SZ 512
#define LVAL_SZ sizeof(lval)


/* Parsers */
mpc_parser_t *Number;
mpc_parser_t *String;
mpc_parser_t *Symbol;
mpc_parser_t *Comment;
mpc_parser_t *Sexpr;
mpc_parser_t *Qexpr;
mpc_parser_t *Expr;
mpc_parser_t *Lispy;

/* Create a buffer */
static char buffer[REPL_BUF];
static size_t memory_allocated = 0;


char* readline(char *prompt) {
  fputs(prompt, stdout);
  fgets(buffer, REPL_BUF, stdin);

  const size_t newSize = strlen(buffer) + 1;
  char *cpy = malloc(newSize);
  strcpy(cpy, buffer);
  cpy[newSize-1] = '\0';

  return cpy;
}

/* Forward decl */
struct lval;
struct lenv;
typedef struct lval lval;
typedef struct lenv lenv;

/* Lisp value */
enum { LVAL_ERR, LVAL_NUM, LVAL_SYM, LVAL_STR, LVAL_FUNC, LVAL_SEXPR, LVAL_QEXPR };

typedef lval *(*lbuiltin)(lenv *, lval *);

/* Lisp value */
struct lval {
    int type;

    /* Basics */
    long num;
    char* err;
    char* sym;
    char* str;

    /* Function */
    lenv *env;
    lval *formals;
    lval *body;
    lbuiltin builtin;
    
    /* Expr */
    int count;
    struct lval **cell;
};

typedef struct entry_t {
    char *key;
    lval *val;
} entry_t;

/* Lisp environment */
struct lenv {
    lenv *parent;
    size_t count, capacity;
    entry_t **entries;
};


lval *lval_copy(lval *val);
void lval_del(lval *val);

entry_t *entry_copy(entry_t *entry) {
    entry_t *copy = (entry_t *)malloc(sizeof(entry_t));
    copy->val = lval_copy(entry->val);
    copy->key = (char *)malloc(strlen(entry->key) + 1);

    strcpy(copy->key, entry->key);
    return copy;
}

void entry_del(entry_t *entry) {
    lval_del(entry->val);

    free(entry->key);
    free(entry);
}

/* ====== HASHING ====== */
/* 
    NOTE: Hashing functions thanks to the crafting interpreters book
*/

int lval_hash(lval *key) {
    int lhash = 2166136261u;
    
    for (size_t i = 0; i < strlen(key->sym); ++i) {
        lhash ^= (int)key->sym[i];
        lhash *= 16777619;
    }
    return lhash;
}

int lenv_find_index(lenv *env, lval *key) {
    int index = lval_hash(key) % env->capacity;

    while(1) {
        if (env->entries[index] == NULL || strcmp(env->entries[index]->key, key->sym) == 0) return index;
        index = (index + 1) % env->capacity;
    }
}

int lenv_insert_value(lenv *env, lval *key, lval *val) {
    int hash_index = lval_hash(key) % env->capacity;
    
    /* Is it a new entry? */
    if (env->entries[hash_index] == NULL) {
        printf("Empty value slot at %d\n", hash_index);
        /* Check for resizing */
        if (env->count + 1 >= env->capacity) {
            const size_t oldCap = env->capacity;
            env->capacity *= 1.75;

            const size_t val_sz = sizeof(entry_t*) * env->capacity;
            env->entries = (entry_t **)realloc(env->entries, val_sz);

            memory_allocated += val_sz - oldCap;

#ifdef DEBUG
            printf("values, bytes allocated :: %lld/%lld values, %zu bytes (symbols + values) [%lld bytes total]\n",
                env->count, 
                env->capacity, 
                val_sz, 
                memory_allocated);
#endif
        }
        /* Increment */
        env->count++;
    } else {
        entry_del(env->entries[hash_index]);
    }

    /* Finally we insert */
    /* Note: we don't care too much if we overwrite some things, as longs as all values make it in */
    entry_t* entry = env->entries[hash_index] = (entry_t *)malloc(sizeof(entry_t));
    entry->key = (char*)malloc(strlen(key->sym) + 1);
    entry->val = lval_copy(val);

    strcpy(entry->key, key->sym);
    return hash_index;
}


char *ltype_name(size_t type) {
    switch(type) {
        case LVAL_FUNC:     return "Function";
        case LVAL_NUM:      return "Number";
        case LVAL_STR:      return "String";
        case LVAL_ERR:      return "Error";
        case LVAL_SYM:      return "Symbol";
        case LVAL_SEXPR:    return "S-Expression";
        case LVAL_QEXPR:    return "Q-Expression";
        default:            return "Unknown";
    }
}


/* Forward decl */
void lval_expr_print(lval *val, char open, char close);
lval *lval_eval(lenv *env, lval *ast);
lval *lval_pop(lval *root, int idx);
lval *lval_take(lval *val, int idx);
lenv *lenv_new();
lenv *lenv_copy(lenv *env);
void lenv_del(lenv *env);


/* Create a new func value */
lval *lval_func(lbuiltin func) {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_FUNC;
    val->builtin = func;
    return val;
}

/* Create a lambda */
lval *lval_lambda(lval *formals, lval *body) {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_FUNC;

    val->builtin = NULL;
    val->env = lenv_new();

    val->formals = formals;
    val->body = body;
    return val;
}

/* Create a new number value */
lval *lval_num(long value) {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_NUM;
    val->num = value;
    return val;
}

/* Create a new string */
lval *lval_str(char *str) {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_STR;
    val->str = (char *)malloc(strlen(str) + 1);
    strcpy(val->str, str);
    return val;
}

/* Create a new error value */
lval *lval_err(char *fmt, ...) {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_ERR;

    /* VARG list */
    va_list va;
    va_start(va, fmt);

    val->err = (char *)malloc(ERR_SZ);

    vsnprintf(val->err, ERR_SZ-1, fmt, va);

    val->err = (char *)realloc(val->err, strlen(val->err) + 1);

    /* VARG cleanup */
    va_end(va);
    return val;
}

/* Construct an lval symbol */
lval *lval_sym(char* sym) {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_SYM;
    val->sym = (char *)malloc(strlen(sym) + 1);
    strcpy(val->sym, sym);
    return val;
}

/* A pointer to a new empty Qexpr lval */
lval *lval_qexpr() {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_QEXPR;
    val->count = 0;
    val->cell = NULL;
    return val;
}

/* A pointer to a new empty Sexpr lval */
lval *lval_sexpr() {
    lval *val = (lval *)malloc(LVAL_SZ);
    val->type = LVAL_SEXPR;
    val->count = 0;
    val->cell = NULL;
    return val;
}

lval *lval_copy(lval *val) {
    lval *copy = (lval *)malloc(LVAL_SZ);
    copy->type = val->type;

    switch(copy->type) {
        /* Copy functions and numbers directly */
        case LVAL_FUNC: {
            if (val->builtin) {
                copy->builtin = val->builtin;
            } else {
                copy->builtin = NULL;
                copy->env = lenv_copy(val->env);
                copy->formals = lval_copy(val->formals);
                copy->body = lval_copy(val->body);
            }
            break;
        }
        case LVAL_NUM: copy->num = val->num; break;

        case LVAL_STR: {
            copy->str = (char *)malloc(strlen(val->str) + 1);
            strcpy(copy->str, val->str);
            break;
        }

        /* Copy strings using malloc and strcpy */
        case LVAL_ERR: {
            copy->err = (char *)malloc(strlen(val->err) + 1);
            strcpy(copy->err, val->err);
            break;
        }

        case LVAL_SYM: {
            copy->sym = (char *)malloc(strlen(val->sym) + 1);
            strcpy(copy->sym, val->sym);
            break;
        }

        /* Copy lists by copying each sub-expr */
        case LVAL_SEXPR:
        case LVAL_QEXPR: {
            copy->count = val->count;
            copy->cell = (lval **)malloc(sizeof(lval*) * val->count);

            for (size_t i = 0; i < val->count; ++i) {
                copy->cell[i] = lval_copy(val->cell[i]);
            }
            break;
        }
    }

    return copy;
}

void lval_del(lval *val) {
    switch(val->type) {
        case LVAL_FUNC: {
            if (!val->builtin) {
                lenv_del(val->env);
                lval_del(val->formals);
                lval_del(val->body);
            }
            break;
        }
        case LVAL_NUM:  break;

        case LVAL_STR: free(val->str); break;

        case LVAL_ERR: free(val->err); break;
        case LVAL_SYM: free(val->sym); break;

        case LVAL_SEXPR:
        case LVAL_QEXPR: {
            for (size_t i = 0; i < val->count; ++i) {
                lval_del(val->cell[i]);
            }
            free(val->cell);
            break;
        }
    }

    
    free(val);
}


/* ====== LENV ======= */
lenv *lenv_new() {
    lenv *env = (lenv *)malloc(sizeof(lenv));
    env->parent = NULL;
    env->count = 0;
    env->capacity = 128;

    const size_t val_sz = sizeof(entry_t*) * env->capacity;
    env->entries = (entry_t **)malloc(val_sz);

    memory_allocated += val_sz;
    return env;
}

lenv *lenv_copy(lenv *env) {
    lenv *copy = (lenv *)malloc(sizeof(lenv));
    copy->parent = env->parent;
    copy->count = env->count;
    copy->capacity = env->capacity;

    const size_t val_sz = sizeof(entry_t*) * env->capacity;
    copy->entries = (entry_t **)malloc(val_sz);
    memory_allocated += val_sz;

    for (size_t i = 0; i < env->count; ++i) {
        copy->entries[i] = entry_copy(env->entries[i]);
    }
    return copy;
}

void lenv_del(lenv *env) {
    for (size_t i = 0; i < env->count; ++i) {
        entry_del(env->entries[i]);
    }

    free(env->entries);
    free(env);
}


lval *lenv_get(lenv *env, lval *key) {
    // TODO: We should hash the key to make ~O(1) fetches/inserts
    int idx = lenv_find_index(env, key);

    entry_t *entry = env->entries[idx];

    if (idx >= 0 && entry && strcmp(entry->key, key->sym) == 0) {
        return lval_copy(entry->val);
    }

    /* Nothing found */
    if (env->parent) {
        return lenv_get(env->parent, key);
    } else {
        return lval_err("unbound symbol '%s'", key->sym);
    }
}

void lenv_put(lenv *env, lval *key, lval *value) {
    int idx = lenv_insert_value(env, key, value);

    if (idx == -1) {
        printf("maaaate");
    }
}

void lenv_def(lenv *env, lval *key, lval *val) {
    while(env->parent) { env = env->parent; }
    lenv_put(env, key, val);
}

/* ====== PARSER ====== */
lval *lval_read_num(mpc_ast_t *ast) {
    errno = 0;
    long val = strtol(ast->contents, NULL, 10);

    return errno != ERANGE ? lval_num(val) : lval_err("invalid number");
}

lval *lval_read_str(mpc_ast_t *ast) {
    ast->contents[strlen(ast->contents)-1] = '\0';

    char *unescaped = (char *)malloc(strlen(ast->contents + 1) + 1);
    strcpy(unescaped, ast->contents + 1);

    unescaped = mpcf_unescape(unescaped);
    lval *str = lval_str(unescaped);
    free(unescaped);
    return str;
}

lval *lval_add(lval *lhs, lval *rhs) {
    lhs->count++;
    lhs->cell = (lval **)realloc(lhs->cell, sizeof(lval*) * lhs->count);
    lhs->cell[lhs->count - 1] = rhs;
    return lhs;
}

lval *lval_read(mpc_ast_t *ast) {
    /* Symbol or number */
    if (strstr(ast->tag, "number")) { return lval_read_num(ast); }
    if (strstr(ast->tag, "string")) { return lval_read_str(ast); }
    if (strstr(ast->tag, "symbol")) { return lval_sym(ast->contents); }

    /* Root expr or Sexpr */
    lval *val = NULL;
    if (strcmp(ast->tag, ">") == 0 || strstr(ast->tag, "sexpr")) { val = lval_sexpr(); }
    if (strstr(ast->tag, "qexpr")) { val = lval_qexpr(); }

    /* Fill with valid expr */
    for (size_t i = 0; i < ast->children_num; ++i) {
        if (
            strcmp(ast->children[i]->contents, "(") == 0 ||
            strcmp(ast->children[i]->contents, ")") == 0 ||
            strcmp(ast->children[i]->contents, "{") == 0 ||
            strcmp(ast->children[i]->contents, "}") == 0 ||
            strcmp(ast->children[i]->tag, "regex") == 0 ||
            strstr(ast->children[i]->tag, "comment")
            ) { continue; }
        
        val = lval_add(val, lval_read(ast->children[i]));
    }
    return val;
}


void lval_print_str(lval *val) {
    char *escaped = (char *)malloc(strlen(val->str) + 1);
    strcpy(escaped, val->str);

    escaped = mpcf_escape(escaped);
    printf("\"%s\"", escaped);
    free(escaped);
}


void lval_print(lval *val) {
    switch(val->type) {
        case LVAL_NUM:      printf("%li", val->num); break;
        case LVAL_STR:      lval_print_str(val); break;
        case LVAL_FUNC: {
            if (val->builtin) {
                printf("<builtin>");
            } else {
                printf("(\\ "); lval_print(val->formals);
                putchar(' '); lval_print(val->body); putchar(')');
            }
            break;
        }
        case LVAL_ERR:      printf("Error: %s\n", val->err); break;
        case LVAL_SYM:      printf("%s", val->sym); break;
        case LVAL_SEXPR:    lval_expr_print(val, '(', ')'); break;
        case LVAL_QEXPR:    lval_expr_print(val, '{', '}'); break;
    }
}

void lval_println(lval *val) {
    lval_print(val);
    printf("\n");
}


/* Print functionality for lval */
void lval_expr_print(lval *val, char open, char close) {
    putchar(open);

    for (size_t i = 0; i < val->count; ++i) {
        lval_print(val->cell[i]);

        if (i < val->count - 1) {
            putchar(' ');
        }
    }

    putchar(close);
}

#define LASSERT(args, cond, fmt, ...) \
    if (!(cond)) { \
        lval *err = lval_err(fmt, ##__VA_ARGS__); \
        lval_del(args); \
        return err; \
    }

#define LASSERT_TYPE(func, args, index, expect) \
  LASSERT(args, args->cell[index]->type == expect, \
    "Function '%s' passed incorrect type for argument %i. Got %s but expected %s.", \
    func, index, ltype_name(args->cell[index]->type), ltype_name(expect))


#define LASSERT_NUM(func, args, num) \
  LASSERT(args, args->count == num, \
    "Function '%s' passed incorrect number of arguments. Got %i but expected %i.", \
    func, args->count, num)


lval *builtin_lambda(lenv *env, lval *val) {
    LASSERT_NUM("\\", val, 2);
    LASSERT_TYPE("\\", val, 0, LVAL_QEXPR);
    LASSERT_TYPE("\\", val, 1, LVAL_QEXPR);

    /* Check only contains symbols */
    for (size_t i = 0; i < val->cell[0]->count; ++i) {
        LASSERT(val, (val->cell[0]->cell[i]->type == LVAL_SYM),
            "Cannot define a non-symbol. Got %s but expected %s.",
            ltype_name(val->cell[0]->cell[i]->type), ltype_name(LVAL_SYM)
        );
    }

    /* Pop first two and pass to lamba */
    lval *formals = lval_pop(val, 0);
    lval *body = lval_pop(val, 0);
    lval_del(val);

    return lval_lambda(formals, body);
}


lval *builtin_head(lenv *env, lval *val) {
    /* Check error conditions */
    LASSERT(val, val->count == 1, 
    "Function 'head' passed too many arguments. "
    "Got %i but expected %i.",
    val->count, 1
    );
    LASSERT(val, val->cell[0]->type == LVAL_QEXPR, 
    "Function 'head' passed incorrect types. "
    "Got %s but expected %s.",
    ltype_name(val->cell[0]->type), ltype_name(LVAL_QEXPR)
    );
    LASSERT(val, val->cell[0]->count != 0, "Function 'head' passed {}");

    /* Take first arg */
    lval *first = lval_take(val, 0);

    /* Delete all other */
    while(first->count > 1) { lval_del(lval_pop(first, 1)); }
    return first;
}

lval *builtin_tail(lenv *env, lval *val) {
    /* Check error conditions */
    LASSERT(val, val->count == 1, 
    "Function 'tail' passed too many arguments. "
    "Got %i but expected %i.",
    val->count, 1
    );
    LASSERT(val, val->cell[0]->type == LVAL_QEXPR, 
    "Function 'tail' passed incorrect types. "
    "Got %s but expected %s.",
    ltype_name(val->cell[0]->type), ltype_name(LVAL_QEXPR)
    );
    LASSERT(val, val->cell[0]->count != 0, "Function 'tail' passed {}");

    /* Take first arg */
    lval *first = lval_take(val, 0);
    lval_del(lval_pop(first, 0));
    return first;
}

lval *builtin_list(lenv *env, lval *val) {
    val->type = LVAL_QEXPR;
    return val;
}

lval *builtin_eval(lenv *env, lval *val) {
    LASSERT(val, val->count == 1, "Function 'eval' passed too many arguments");
    LASSERT(val, val->cell[0]->type == LVAL_QEXPR, "Function 'eval' passed incorrect argument");

    lval *new = lval_take(val, 0);
    new->type = LVAL_SEXPR;
    return lval_eval(env, new);
}

lval *lval_join(lenv *env, lval *lhs, lval *rhs) {
    while(rhs->count) {
        lhs = lval_add(lhs, lval_pop(rhs, 0));
    }

    lval_del(rhs);
    return lhs;
}

lval *builtin_join(lenv *env, lval *val) {
    for (size_t i = 0; i < val->count; ++i) {
        LASSERT(val, val->cell[i]->type == LVAL_QEXPR, "Function 'join' passed incorrect type");
    }

    lval *new = lval_pop(val, 0);

    while(val->count) {
        new = lval_join(env, new, lval_pop(val, 0));
    }

    lval_del(val);
    return new;
}

lval *builtin_op(lenv *env, lval *val, char *op) {
    /* Ensure all are numbers */
    for (size_t i = 0; i < val->count; ++i) {
        LASSERT_TYPE(op, val, i, LVAL_NUM);
    }

    lval *lhs = lval_pop(val, 0);

    if (*op == '-' && val->count == 0) {
        lhs->num = -lhs->num;
    }

    int err = 0;

    while(!err && val->count > 0) {
        lval *rhs = lval_pop(val, 0);

        switch(*op) {
            case '+': lhs->num += rhs->num; break;
            case '-': lhs->num -= rhs->num; break;
            case '*': lhs->num *= rhs->num; break;
            case '/': {
                if (rhs->num == 0) {
                    lval_del(lhs); lval_del(rhs);
                    lhs = lval_err("Division by 0");
                    err = 1;
                    break;
                }
                lhs->num /= rhs->num;
                break;
            }
        }

        lval_del(rhs);
    }

    lval_del(val);
    return lhs;
}


lval *builtin_var(lenv *env, lval *val, char* func) {
    LASSERT_TYPE(func, val, 0, LVAL_QEXPR);

    lval *syms = val->cell[0];
    for (size_t i = 0; i < syms->count; ++i) {
        LASSERT(val, (syms->cell[i]->type == LVAL_SYM),
            "Function '%s' cannot define non-symbol. "
            "Got %s but expected %s.", func,
            ltype_name(syms->cell[i]->type),
            ltype_name(LVAL_SYM)
        );
    }

    LASSERT(val, (syms->count == val->count - 1),
        "Function '%s' passed too many arguments for symbols. "
        "Got %i but expected %i.", func, syms->count, val->count-1
    );

    for (size_t i = 0; i < syms->count; ++i) {
        /* if 'def' define in global. if 'put' define in local */
        if (strcmp(func, "def") == 0) {
            lenv_def(env, syms->cell[i], val->cell[i+1]);
        }

        if (strcmp(func, "=") == 0) {
            lenv_put(env, syms->cell[i], val->cell[i+1]);
        }
    }
    lval_del(val);
    return lval_sexpr();
}


lval *builtin_load(lenv *env, lval *val) {
    LASSERT_NUM("load", val, 1);
    LASSERT_TYPE("load", val, 0, LVAL_STR);

    /* Parse file given by string */
    mpc_result_t r;

    if (mpc_parse_contents(val->cell[0]->str, Lispy, &r)) {
        /* Read contents */
        lval *expr = lval_read(r.output);
        mpc_ast_delete(r.output);

        /* Evaluate */
        while(expr->count) {
            lval *res = lval_eval(env, lval_pop(expr, 0));
            /* If error */
            if (res->type == LVAL_ERR) { lval_println(res); }
            lval_del(res);
        }

        /* Cleanup */
        lval_del(expr);
        lval_del(val);

        return lval_sexpr();
    } else {
        /* Error */
        char *err_msg = mpc_err_string(r.error);
        mpc_err_delete(r.error);

        /* New error message */
        lval *err = lval_err("Could not load library %s", err_msg);
        free(err_msg);
        lval_del(val);

        return err;
    }
}

/* ====== Register Functions ====== */
lval *builtin_add(lenv *env, lval *val) {
    return builtin_op(env, val, "+");
}

lval *builtin_sub(lenv *env, lval *val) {
    return builtin_op(env, val, "-");
}

lval *builtin_mul(lenv *env, lval *val) {
    return builtin_op(env, val, "*");
}

lval *builtin_div(lenv *env, lval *val) {
    return builtin_op(env, val, "/");
}

lval *builtin_def(lenv *env, lval *val) {
    return builtin_var(env, val, "def");
}

lval *builtin_put(lenv *env, lval *val) {
    return builtin_var(env, val, "=");
}

lval *builtin_ord(lenv *env, lval *val, char *op) {
    LASSERT_NUM(op, val, 2);
    LASSERT_TYPE(op, val, 0, LVAL_NUM);
    LASSERT_TYPE(op, val, 1, LVAL_NUM);

    int r;
    if (strcmp(op, ">") == 0) {
        r = (val->cell[0]->num > val->cell[1]->num);
    }

    if (strcmp(op, "<") == 0) {
        r = (val->cell[0]->num < val->cell[1]->num);
    }

    if (strcmp(op, ">=") == 0) {
        r = (val->cell[0]->num >= val->cell[1]->num);
    }

    if (strcmp(op, "<=") == 0) {
        r = (val->cell[0]->num <= val->cell[1]->num);
    }

    lval_del(val);
    return lval_num(r);
}

lval *builtin_gt(lenv *env, lval *val) {
    return builtin_ord(env, val, ">");
}

lval *builtin_lt(lenv *env, lval *val) {
    return builtin_ord(env, val, "<");
}

lval *builtin_ge(lenv *env, lval *val) {
    return builtin_ord(env, val, ">=");
}

lval *builtin_le(lenv *env, lval *val) {
    return builtin_ord(env, val, "<=");
}

int lval_eq(lval *lhs, lval *rhs) {
    /* Different types are always unequal */
    if (lhs->type != rhs->type) { return 0; }

    switch(lhs->type) {
        case LVAL_NUM:  return lhs->num == rhs->num;
        case LVAL_STR: return strcmp(lhs->str, rhs->str) == 0;

        /* Compare strings */
        case LVAL_ERR: return strcmp(lhs->err, rhs->err) == 0;
        case LVAL_SYM: return strcmp(lhs->sym, rhs->sym) == 0;

        case LVAL_FUNC: {
            if (lhs->builtin || rhs->builtin) {
                return lhs->builtin == rhs->builtin;
            } else {
                return lval_eq(lhs->formals, rhs->formals) && lval_eq(lhs->body, rhs->body);
            }
        }

        case LVAL_QEXPR:
        case LVAL_SEXPR: {
            if (lhs->count != rhs->count) { return 0; }

            for (int i = 0; i < lhs->count; ++i) {
                if (!lval_eq(lhs->cell[i], rhs->cell[i])) { return 0; }
            }
            return 1;
        }
    }
    return 0;
}


lval *builtin_cmp(lenv *env, lval *val, char *op) {
    LASSERT_NUM(op, val, 2);

    int r;
    if (strcmp(op, "==") == 0) {
        r = lval_eq(val->cell[0], val->cell[1]);
    }

    if (strcmp(op, "!=") == 0) {
        r = !lval_eq(val->cell[0], val->cell[1]);
    }
    lval_del(val);
    return lval_num(r);
}

lval *builtin_eq(lenv *env, lval *val) {
    return builtin_cmp(env, val, "==");
}

lval *builtin_ne(lenv *env, lval *val) {
    return builtin_cmp(env, val, "!=");
}

lval *builtin_if(lenv *env, lval *val) {
    LASSERT_NUM("if", val, 3);
    LASSERT_TYPE("if", val, 0, LVAL_NUM);
    LASSERT_TYPE("if", val, 1, LVAL_QEXPR);
    LASSERT_TYPE("if", val, 2, LVAL_QEXPR);

    /* Mark both expressions as evaluable */
    lval *result;
    val->cell[1]->type = LVAL_SEXPR;
    val->cell[2]->type = LVAL_SEXPR;

    if (val->cell[0]->num) {
        /* If is true */
        result = lval_eval(env, lval_pop(val, 1));
    } else {
        result = lval_eval(env, lval_pop(val, 2));
    }

    lval_del(val);
    return result;
}

lval *builtin_print(lenv *env, lval *val) {
    /* Print every arg */
    for (size_t i = 0; i < val->count; ++i) {
        lval_print(val->cell[i]); putchar(' ');
    }

    putchar('\n');
    lval_del(val);

    return lval_sexpr();
}

lval *builtin_error(lenv *env, lval *val) {
    LASSERT_NUM("error", val, 1);
    LASSERT_TYPE("error", val, 0, LVAL_STR);

    lval *err = lval_err(val->cell[0]->str);

    lval_del(val);
    return err;
}

void lenv_add_builtin(lenv *env, char* name, lbuiltin func) {
    lval *key = lval_sym(name);
    lval *val = lval_func(func);

    lenv_put(env, key, val);
    lval_del(key); lval_del(val);
}


void lenv_add_builtins(lenv *env) {
    lenv_add_builtin(env, "\\", builtin_lambda);
    lenv_add_builtin(env, "def", builtin_def);
    lenv_add_builtin(env, "=", builtin_put);

    /* String functions */
    lenv_add_builtin(env, "load", builtin_load);
    lenv_add_builtin(env, "error", builtin_error);
    lenv_add_builtin(env, "print", builtin_print);

    /* Comparisons */
    lenv_add_builtin(env, "if", builtin_if);
    lenv_add_builtin(env, "==", builtin_eq);
    lenv_add_builtin(env, "!=", builtin_ne);
    lenv_add_builtin(env, ">",  builtin_gt);
    lenv_add_builtin(env, "<",  builtin_lt);
    lenv_add_builtin(env, ">=", builtin_ge);
    lenv_add_builtin(env, "<=", builtin_le);

    /* Variable functions */
    lenv_add_builtin(env, "def", builtin_def);

    /* List functions */
    lenv_add_builtin(env, "list", builtin_list);
    lenv_add_builtin(env, "head", builtin_head);
    lenv_add_builtin(env, "tail", builtin_tail);
    lenv_add_builtin(env, "eval", builtin_eval);
    lenv_add_builtin(env, "join", builtin_join);

    /* Mathmatical functions */
    lenv_add_builtin(env, "+", builtin_add);
    lenv_add_builtin(env, "-", builtin_sub);
    lenv_add_builtin(env, "*", builtin_mul);
    lenv_add_builtin(env, "/", builtin_div);
}


lval *lval_call(lenv *env, lval *func, lval *val) {
    /* If builtin then apply that */
    if (func->builtin) { return func->builtin(env, val); }

    /* Record arg count */
    int given = val->count;
    int total = func->formals->count;

    /* While arguments still remain to be processed */
    while(val->count) {
        /* If we've run out of formal args */
        if (func->formals->count == 0) {
            lval_del(val); return lval_err(
                "Function passed too many arguments. "
                "Got %i but expected %i.",
                given, total
            );
        }

        /* Pop symbol from formals */
        lval *sym = lval_pop(func->formals, 0);

        /* Variable argument count */
        if (strcmp(sym->sym, "&") == 0) {
            /* Ensure & is followed by another symbol */
            if (func->formals->count != 1) {
                lval_del(val);
                return lval_err("Function format invalid. "
                    "Symbol '&' not followd by a single symbol.");
            }

            /* Next formal will be bound to the remaining args */
            lval *nsym = lval_pop(func->formals, 0);
            lenv_put(func->env, nsym, builtin_list(env, val));
            lval_del(sym); lval_del(nsym);
            break;
        }

        /* Pop next arg */
        lval *arg = lval_pop(val, 0);

        /* Bind a copy to the functions environment */
        lenv_put(func->env, sym, arg);

        /* Cleanup */
        lval_del(sym); lval_del(arg);
    }

    /* List is bound and can be cleaned up */
    lval_del(val);

    /* If '&' in formal list bind to empty list */
    if (func->formals->count > 0 && strcmp(func->formals->cell[0]->sym, "&") == 0) {
        /* Check if & is not passed invalidly */
        if (func->formals->count != 2) {
            return lval_err("Function format invalid. "
                "Symbol '&' not followed by a single symbol.");
        }

        /* Pop and delete & */
        lval_del(lval_pop(func->formals, 0));

        /* Pop next and create empty list */
        lval *sym = lval_pop(func->formals, 0);
        lval *arg = lval_qexpr();

        /* Bind to environment */
        lenv_put(func->env, sym, arg);
        lval_del(sym); lval_del(arg);
    }

    if (func->formals->count == 0) {
        /* Set parent environment */
        func->env->parent = env;

        /* Evaluate and return */
        return builtin_eval(
            func->env, lval_add(lval_sexpr(), lval_copy(func->body))
        );
    } else {
        /* Return partially evaluated function */
        return lval_copy(func);
    }
}


lval *lval_pop(lval *root, int idx) {
    /* Find item */
    lval *val = root->cell[idx];

    /* Shift memory after idx */
    memmove(&root->cell[idx], &root->cell[idx+1], sizeof(lval*) * (root->count - idx - 1));

    /* Reallocate */
    root->count--;
    root->cell = (lval **)realloc(root->cell, sizeof(lval*) * root->count);
    return val;
}


lval *lval_take(lval *val, int idx) {
    lval *new = lval_pop(val, idx);
    lval_del(val);
    return new;
}


lval *lval_eval_sexpr(lenv *env, lval *val) {
    /* Evaluate children */
    for (size_t i = 0; i < val->count; ++i) {
        val->cell[i] = lval_eval(env, val->cell[i]);
    }

    /* Error checking */
    for (size_t i = 0; i < val->count; ++i) {
        if (val->cell[i]->type == LVAL_ERR) { return lval_take(val, i); }
    }

    if (val->count == 0) { return val; }
    if (val->count == 1) { return lval_take(val, 0); }

    /* Ensure first is a function after eval */
    lval *f = lval_pop(val, 0);
    if (f->type != LVAL_FUNC) {
        lval *err = lval_err(
            "S-Expression starts with incorrect type. "
            "Got %s but expected %s.",
            ltype_name(f->type), ltype_name(LVAL_FUNC)
        );
        lval_del(val); lval_del(f);
        return err;
    }

    /* Call function to get result */
    lval *result = lval_call(env, f, val);
    lval_del(f);
    return result;
}


lval *lval_eval(lenv *env, lval *val) {
    switch(val->type) {
        /* Variable */
        case LVAL_SYM: {
            lval *var = lenv_get(env, val);
            lval_del(val);
            return var;
        }

        /* Evaluate Sexpr */
        case LVAL_SEXPR:    return lval_eval_sexpr(env, val);
        /* Other types remain*/
        default: return val;
    }
}


int main(int argc, char **argv) {
    /* Create parsers */
    Number = mpc_new("number");
    String = mpc_new("string");
    Symbol = mpc_new("symbol");
    Comment = mpc_new("comment");
    Sexpr = mpc_new("sexpr");
    Qexpr = mpc_new("qexpr");
    Expr = mpc_new("expr");
    Lispy = mpc_new("lispy");

    /* Define parsers with language */
    mpca_lang(MPCA_LANG_DEFAULT,
        "                                                \
            number  : /-?[0-9]+/ ;                       \
            symbol  : /[a-zA-Z0-9_+\\-*\\/\\\\=<>!&]+/ ; \
            string  : /\"(\\\\.|[^\"])*\"/ ;             \
            comment : /;[^\\r\\n]*/ ;                    \
            sexpr   : '(' <expr>* ')' ;                  \
            qexpr   : '{' <expr>* '}' ;                  \
            expr    : <number>  | <symbol> | <string>    \
                    | <comment> | <sexpr>  | <qexpr>;    \
            lispy   : /^/ <expr>* /$/ ;                  \
        ",
        Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);

    
    /* Setup an environment */
    lenv *env = lenv_new();
    lenv_add_builtins(env);

    /* Parse user input */
    if (argc == 1) {
        while(1) {
            char *input = readline("lispy> ");

            mpc_result_t ast;

            if (mpc_parse("<stdin>", input, Lispy, &ast)) {
#ifdef DEBUG
                clock_t start, end;
                double cpu_time_used;

                start = clock();
#endif
                lval *result = lval_eval(env, lval_read(ast.output));
                lval_println(result);
                lval_del(result);

#ifdef DEBUG
                end = clock();
                cpu_time_used = ((double)(end - start)) / CLOCKS_PER_SEC;
                printf("cpu time used :: %f\n", cpu_time_used);
#endif

                mpc_ast_delete(ast.output);
            } else {
                mpc_err_print(ast.error);
                mpc_err_delete(ast.error);
            }

            /* Cleanup user input */
            free(input);
        }
    } 
    else if(argc >= 2) {
        /* Loop every file */
        for (size_t i = 1; i < argc; ++i) {
            lval *args = lval_add(lval_sexpr(), lval_str(argv[i]));

            /* Pass to built-in and load */
            lval *result = builtin_load(env, args);

            /* Check for error */
            if (result->type == LVAL_ERR) { lval_print(result); }
            lval_del(result);
        }
    }
    else {
        printf("usage: lispy [source]\n");
    }

    lenv_del(env);

    /* Cleanup parser */
    mpc_cleanup(8, Number, Symbol, String, Comment, Sexpr, Qexpr, Expr, Lispy);
    return 0;
}