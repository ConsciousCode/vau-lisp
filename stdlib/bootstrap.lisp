;; Bootstrapping a lisp using wasm and vau calculus
;; Vau calculus defines a universal special form $vau that can be used to define all other special forms
;; The core type hierarchy is combiner <: operative <: applicative
;;
;; Primitives:
;; - `$def!` defines a variable in the current environment
;; - `$vau` constructs an operative which accepts its arguments unevaluated
;; - `wrap` constructs an applicative from a combiner, evaluating the arguments
;; - `unwrap` unwraps a wrapped combiner; (unwrap (wrap f)) = f
;;
;; So we can evaluate expressions in the interpreter without wasm, these other primitives are provided:
;; - `eval` invokes the interpreter on an expression with a given environment
;; - `$cond` operative evaluates conditions in order until one is true
;;   - We use $cond instead of if because it's easier to implement if in $cond than vice versa,
;;     and it's not much more effort to implement $cond in the interpreter
;; - `+` a dynamically typed applicative that adds its arguments
;; - `eq?` strict identity applicative

(($vau-monad _ old
    (() () . (()
        ('$def! . ($vau-monad xs env
            (() () . (()
                ((car xs) . (eval (car (cdr xs))))
                . env ))))
        ('$vau . ($vau-monad xs env
            ))
        . old
    ))))

;; Define definition using vau to capture the environment
(($vau _ *global*
    (setcar! *global* (()
        ;; Environments are lists of lists of name-value pairs
        (() '$def! . ($vau xs env
            (setcar! env (()
                (() (car xs) . (eval (car (cdr xs)) env))
                . (car env) ))))
        . (car *global*) ))))

($def! nil ())
($def! true  (eq? nil  nil))
($def! false (eq? true nil))
($def! list (wrap ($vau x _ x)))
($def! quote ($vau xs _ (car xs)))
($def! #ignore '#ignore)

($def! $lambda ($vau xs e
    (wrap (eval (list $vau . xs) (list () . e))) ))

($def! $defn! ($vau (name ptree . body) env
    (eval (list $def! name (list $lambda ptree . body)) env) ))

($defn! null? xs (eq? (car xs) nil))
($defn! cons xs (list (car xs) . (cadr xs)))

($defn!   cadr           (car (cdr (car xs)))  )
($defn!   cddr           (cdr (cdr (car xs)))  )
($defn!  caddr      (car (cdr (cdr (car xs)))) )
($defn! cadddr (car (cdr (cdr (cdr (car xs))))))

($def! begin ($vau body e
    ((eval (list $vau #ignore #ignore . body) e)) ))

($def! and ($vau xs e ;; Returns the first falsey value, or the last truthy one
    ($if (null? xs) true (begin
        ($def! ex (eval (car xs) e)
        ($def! xs       (cdr xs)  )
        ($if (null? xs) ex
            ($if ex
                (eval (cons and xs) e)
                ex) ))))))
($def! or ($vau xs e ;; Returns the first truthy value, or the last falsey one
    ($if (null? xs) false (begin
        ($def! ex (eval (car xs) e))
        ($def! xs       (cdr xs)   )
        ($if (null? xs) ex
            ($if ex ex
                (eval (cons or xs) e) ))))))

($def! cond ($vau cs env
    ($if (null? cs) () ;; (cond) == ()
        (eval
            ($if (or (eq? cs 'else) (eval (car cs) env))
                           (cadr cs)
                (cons cond (cddr cs)) )
            env ))))

($def! $set! ($vau tnv env
    (eval (list $def! (cadr tnv) (list (unwrap eval) (caddr tnv) env))
        (eval (car tnv) env))))

($defn! apply (appv args)
    (eval (cons (unwrap appv) args) (make-environment)))

($defn! concat xss
    ($defn! cat xs-ys
        ($def! xs ( car xs-ys))
        ($def! ys (cadr xs-ys))
        ($if (null? xs) ys
            (cons (car xs) (cat (cdr xs) ys))))
    ($if (null? xss) ()
        (cat (car xss) (apply concat (cdr xss)))))

($def! #fail '#fail)

($defn! unpack ns-vs
    ($def! ns ( car ns-vs))
    ($def! vs (cadr ns-vs))
    (print "ns vs" ns vs)
    (cond
        ;; Unpack cons
        (cons? ns) ($if (cons? vs) (begin
            ($def! left (unpack (car ns) (car vs)))
            ($if (eq? left #fail)
                #fail
                (begin
                    ($def! right (unpack (cdr ns) (cdr vs)))
                    ($if (eq? right #fail)
                        #fail
                        (concat left right) ))))
            #fail)
        
        ;; Symbol always matched or ignored
        (symbol? ns) ($if (eq? ns #ignore)
            ()
            (list (cons ns vs)) )
        
        ;; Constant comparison
        (eq? ns vs) ()
        
        else #fail ))

($def! match ($vau ns-vs env
    ;; We want to only assign if there's an actual match
    ;; First, depth-first in-order convert into a pair of lists for names and values
    ($def! ns-vs (unpack (car ns-vs) (eval (cadr ns-vs) env)))
    (print ns-vs)
    ($if (eq? ns-vs #fail)
        false  ;; Match failed
        (begin ;; Match succeeded, define everything
            (eval (cons begin (map
                ($lambda nv (print "map" nv) (set! env (caar nv) (cdar nv)))
                ns-vs)) env)
            true)) ))

;; Execute and return code based on which is a match
($def! which ($vau (expr . m-vs) env
    (if (match (m v . m-vs) m-vs)
        (if (eq? m (quote else)) (eval v env))
        (eval (cond
            (eq? m (quote else)) v
            (match m expr) v
            else (list which . m-vs)
            v (list which . m-vs)) env)
        ()) )) ;; Default to nil

;; vau which unpacks a list of symbols
($def! $vau-list ($vau xs e
    ($def! ptree ( car xs))
    ($def! penv  (cadr xs))
    ($def! body  (cddr xs))
    ($def! )
    (if ())
    ($cond
        ((cons? xs) )
        ((null? xs) . ($vau () e)))
    (eval (list $vau )))

;; Define some easy but high-value operatives
($def! if ($vau (c t f) e
    ($cond
        ((eval c e) . (eval t e))
        (     true  . (eval f e)) )))

;; list can't be a defn because it's a dependency of lambda
($def! lambda ($vau (ptree . body) env
    (wrap (eval (list $vau ptree #ignore . body) env)) ))
($def! begin ($vau body #ignore (lambda () . body)))
($def! defn ;; Syntax sugar for ($def! name (lambda ptree . body))
    ($vau (name ptree . body) env
        (eval (list $def! name (list lambda ptree . body)) env) ))

;; Now we have defn so define a few utilities
(defn not (x) (eq? x false))
(defn ne? (x y) (not (eq? x y)))
(defn null? (x) (eq? x nil))
(defn combiner? (x) ($cond
    ((applicative? x) . true )
    ((  operative? x) . true )
    (        true     . false) ))
(defn id (x . #ignore) x)
(defn apply (appv args)
    (eval (cons (unwrap appv) args) (make-environment)))
(defn const (c) (lambda #ignore c))


(defn foldl (f b xs)
    (which xs
        (x . xs) (foldl (f b x) xs)
        else ()))

(defn foldr (f b xs)
    (defn go (xs acc) (which xs
        (x . xs) (go xs (cons (f x) acc))
        else acc))
    (go xs b))

(defn map (f xs)
    (if (null? xs) nil
        (cons
            (    f (head xs))
            (map f (tail xs)) )))
($def! $let ($vau (bindings . body) env
    (eval (cons
        (list lambda (map head bindings) . body)
        (map tail bindings)) env) ))

;; Evaluate , and @, as operatives, leaving the wasm untouched
($def! module ($vau mod env
    (defn map-unquote (x)
        (if (cons? x)
            (cond
                ((eq? (head x) (quote , )) . (eval (head (tail x)) env))
                ((eq? (head x) (quote ,@)) . (eval (tail (tail x)) env))
            (if (eq? (head x) (quote ,))
                (eval (tail x) env)
                (cons
                    (map-unquote (head x))
                    (map-unquote (tail x)) )))
            x ))
    (map-unquote mod) ))

($def! heap-size 1024)

($def! c_newline   10)
($def! c_space     20)
($def! c_lparen    40)
($def! c_rparen    41)
($def! c_dot       46)
($def! c_semicolon 59)

(module
    (global $source i32)
    (global $env i32 (i32.const (, c_heap-size)))
    (global $atoms i32 (i32.const 0))
    (global $cells i32 (i32.const (, c_heap-size)))
    (global $buffer i32 (i32.const (, c_heap-size)))
    (func quote 
    
    (func $read (export "read") (param $s i32) (result i32)
        (local $c i32)
        (local.set $c (i32.load_8_u (local.get $s)))
        (if (i32.eq (local.get $c) (, c_lparen))
            (then ;; list
                (if (i32.eq (call $peek) (, c_rparen))
                    (then
                        (i32.const 0)
                        (return) ))    
            (else ;; atom
                (if (i32.eq (local.get $c) (, c_quote))
                    (then (call quote))
                    (else (call $atom )) )))
        (return))
    
    
)