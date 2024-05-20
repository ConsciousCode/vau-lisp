;; This prelude is an exercise in frugality
;; I wanted to try to implement powerful features on a few very basic primitives
;; The primitives:
;; - def! - definition of one name to one value
;; - eq? - identity comparison of just two values
;; - $vau - binds parameters to a single name, unpacking is manual
;; - wrap - wrap a combiner as an applicative
;; - unwrap - remove an applicative wrapping
;; - car/cdr - operators for decomposing pairs

;; Note: #ignore doesn't actually *do* anything until way later when we define
;;  pattern matching. Before that point, it's merely a naming convention.
(def! "quote" ($vau x #ignore (car x)))
(def! 'list (wrap ($vau x #ignore x)))

(def! '$def! ($vau nv env
    (eval (list def! (display (car nv)) (list (unwrap eval) (car (cdr nv)) env)) env) ))

($def! #inert ($def! nil ())) ;; REPL displays nothing
($def! true  (eq? nil  nil))
($def! false (eq? true nil))
($def! #ignore '#ignore)
($def! $current-environment ($vau #ignore e e))

($def! begin ($vau body env
    ((eval (list $vau #ignore #ignore . body) env)) ))

($def! $defn! ($vau npb env
    ($def! name (car npb))
    ($def! pb   (cdr npb))
    ($def! parm (car  pb))
    ($def! body (cdr  pb))
    (eval (list $def! name (list wrap (list $vau parm #ignore . body))) env) ))

($defn! null? x (eq? nil . x))

($defn!  caar xs      (car (car . xs)) )
($defn!  cadr xs      (car (cdr . xs)) )
($defn!  cdar xs      (cdr (car . xs)) )
($defn!  cddr xs      (cdr (cdr . xs)) )
($defn! caddr xs (car (cdr (cdr . xs))))

($def! if ($vau ctf env
    (eval
        (select (eval (car ctf) env)
            ( cadr ctf)
            (caddr ctf) )
        env )))

($defn! len= ls-len
    ($def! ls  (car ls-len))
    ($def! len (cadr ls-len))
    ($if (null? ls)
        (eq? len 0)
        (len= (cdr ls) (- len 1)) ))

($defn! length ls-
    ($def! ls (car ls-))
    ($if (null? ls) 0 (+ 1 (length (cdr ls)))) )

#;($def! if ($vau ctf env
    ($if (len= ctf 3)
        (eval (list $if . ctf) env)
        (error "if applied to too few arguments:" (display ctf)) )))

($defn! apply appv-arg-env static-env
    ($def! appv  (car appv-arg-env))
    ($def! arg  (cadr appv-arg-env))
    ($def! env (caddr appv-arg-env))
    (eval (cons (unwrap appv) arg)
        (if (null? opt)
            (make-environment)
            (car opt) )))

;; Concatenate exactly two lists
($defn! concat2 xs-ys
    ($def! xs ( car xs-ys))
    ($def! ys (cadr xs-ys))
    (if (null? xs)
        ys
        (cons (car xs) (concat2 (cdr xs) ys)) ))

;; Unpacks values into parameter trees, returning a list of name-value pairs
;; (ns vs) -> ((n . v)*) | #inert
($defn! unpack ns-vs
    ($def! ns ( car ns-vs))
    ($def! vs (cadr ns-vs))
    (if (cons? ns)
        (if (cons? vs)
            (begin
                ($def! left (unpack (car ns) (car vs)))
                (if (eq? left #inert)
                    #inert
                    (begin
                        ($def! right (unpack (cdr ns) (cdr vs)))
                        (if (eq? right #inert)
                            #inert
                            (concat2 left right) ))))
            #inert)
        (if (symbol? ns)
            (if (eq? ns #ignore)
                ()
                (list (cons ns vs)) )
            (if (eq? ns vs)
                () #inert ))))

;; Set a name to a value in an environment
;; (this is very hard to inline correctly)
($def! set! ($vau nvt env
    ($def! name   (  car nvt))
    ($def! value  ( cadr nvt))
    ($def! target (caddr nvt))
    (eval (list $def! (eval name env) (list (unwrap eval) value env))
        (eval target env))))

($defn! assert cond-msg
    (if (car cond-msg)
        #inert
        (error (cadr cond-msg)) ))

;; Takes a list of name-value pairs and assigns them in an environment
($defn! set-list! nvs-e
    ($def! nvs ( car nvs-e))
    ($def! env (cadr nvs-e))
    (if (null? nvs)
        ()
        (begin
            ($def! nv  (car nvs))
            (set! (car nv) (cdr nv) env)
            (set-list! (cdr nvs) env) )))

;; Returns if the pattern matched and assigns the names if it did
($def! match ($vau ns-vs env
    ;; We want to only assign if there's an actual match
    ;; First, depth-first in-order convert into a pair of lists for names and values
    ($def! nvs (unpack (car ns-vs) (eval (cadr ns-vs) env)))
    (if (eq? nvs #inert)
        false  ;; Match failed
        (begin ;; Match succeeded, define everything
            (set-list! nvs env)
            true)) ))

;; We finally have enough machinery to redefine our primitives to be nicer

($def! vau ($vau peb static-env
    ($def! ptree (car peb))
    ($def! penv (cadr peb))
    ($def! body (cddr peb))
    ($vau args env
        ($def! local-env (make-environment static-env))
        (if (eval (list match ptree (cons (unwrap list) args)) local-env)
            (begin
                (set! penv env local-env)
                (eval (cons begin body) local-env))
            (error "Pattern matching failed:" (display ptree) "got" (display args)) ))))

;; Now we have parameter unpacking
($def! lambda (vau (ptree . body) env
    (wrap (eval (list vau ptree #ignore . body) env)) ))

($def! defvau (vau (name . rest) env
    (eval (list $def! name (list vau . rest)) env) ))

($def! defn (vau (name ptree . body) env
    (eval (list $def! name (list lambda ptree . body)) env) ))

($def! def (vau (ptree values) env
    (if (eval (list match ptree values) env)
        #inert
        (error "Pattern matching failed") )))

($def! and ($vau xs e ;; Returns the first falsey value, or the last true one
    (if (null? xs) true (begin
        ($def! ex (eval (car xs) e))
        ($def! xs       (cdr xs)   )
        (if (null? xs)
            ex
            (if ex
                (eval (cons and xs) e)
                ex) )))))
($def! or ($vau xs e ;; Returns the first truthy value, or the last falsey one
    (if (null? xs) false (begin
        ($def! ex (eval (car xs) e))
        ($def! xs       (cdr xs)   )
        (if (null? xs) ex
            (if ex ex
                (eval (cons or xs) e) ))))))

(defvau cond patterns env
    (defn go (ps)
        (if (null? ps)
            #inert
            (if (or (eq? (car ps) 'else) (eval (car ps) env))
                (eval (cadr ps) env)
                (go (cddr ps)) )))
    (go patterns) )

(defn foldl (f b xs)
    (if (null? xs)
        b
        (foldl f (f b (car xs)) (cdr xs)) ))

(defn foldr (f b xs)
    (if (null? xs)
        b
        (f (car xs) (foldr f b (cdr xs))) ))

(defn map (f xs)
    (foldr (lambda (x rest) (cons (f xs) rest)))
    () xs)

($def! into (wrap ($vau comb-env outer-env
    ($def! comb ( car comb-env))
    ($def! env  (cadr comb-env))
    ($vau args #ignore
        (eval (cons comb args) env)))))
#;(
;; Execute a combiner in a given environment

($defn!   caar xs           (car (car (car xs)))  )
($defn!   cadr xs           (car (cdr (car xs)))  )
($defn!   cdar xs           (cdr (car (car xs)))  )
($defn!   cddr xs           (cdr (cdr (car xs)))  )
($defn!  caddr xs      (car (cdr (cdr (car xs)))) )
($defn! cadddr xs (car (cdr (cdr (cdr (car xs))))))

($defn! list* args
    ($defn! go xs
        ($def! h (car xs))
        ($def! t (cdr xs))
        (if (null? t)
            h
            (cons h (go . t)) ))
    (if (null? args)
        (error "arity mismatch: list* got 0 arguments")
        (go . args) ))

($defn! apply (appv args)
    (eval (cons (unwrap appv) args) (make-environment)))

($defn! concat xss
    ($defn! cat xs-ys
        ($def! xs ( car xs-ys))
        ($def! ys (cadr xs-ys))
        (if (null? xs) ys
            (cons (car xs) (cat (cdr xs) ys))))
    (if (null? xss) ()
        (cat (car xss) (apply concat (cdr xss)))))

($def! cond ($vau cs env
    (if (null? cs) () ;; (cond) == ()
        (eval
            (if (eq? (car cs) 'else)
                (cadr cs)
                (if (eval (car cs) env)
                    (cadr cs)
                    (cons cond (cddr cs)) ))
            env ))))

($defn! map (f xs)
    (if (null? xs) nil
        (cons
            (    f (car xs))
            (map f (cdr xs)) )))

($def! $lambda ($vau ptree-body env
    ($def! ptree (car ptree-body))
    ($def! body  (cdr ptree-body))
    (wrap (eval (list $vau ptree #ignore . body) env))))

($def! $let ($vau (bindings . body) env
    (id (cons
        (list $lambda (map car bindings) . body)
        (map cdr bindings)) env)))

($defn! id (x . #ignore) x)
($defn! const (c) ($lambda #ignore c))
;($defn! zip ())
)