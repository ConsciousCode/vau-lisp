;; This prelude is an exercise in frugality
;; I wanted to try to implement powerful features on a few very basic primitives
;; The primitives:
;; - def! - definition of one name to one value
;; - eq? - identity comparison of just two values
;; - $vau - binds parameters to a single name, unpacking is manual
;; - wrap - wrap a combiner as an applicative
;; - unwrap - remove an applicative wrapping
;; - car/tail - operators for decomposing pairs

;; Note: #ignore doesn't actually *do* anything until way later when we define
;;  pattern matching. Before that point, it's merely a naming convention.
(def! "quote" ($vau x #ignore (car x)))
(def! "list" (wrap ($vau x #ignore x)))

(def! "$def!" ($vau nv env
    (eval (list def!
        (display (head nv))
        (list (unwrap eval) (head (tail nv)) env) ) env) ))

($def! #inert ($def! nil ())) ;; REPL displays nothing
($def! true  (eq? nil  nil))
($def! false (eq? true nil))
($def! #ignore '#ignore)
($def! $current-environment ($vau #ignore e e))

($def! begin ($vau body env
    ((eval (list $vau #ignore #ignore . body) env)) ))

($def! $defn! ($vau npb env
    ($def! name (head npb))
    ($def! pb   (tail npb))
    ($def! parm (head  pb))
    ($def! body (tail  pb))
    (eval (list $def! name (list wrap (list $vau parm #ignore . body))) env) ))

($defn! null? x (eq? nil . x))

($def! if ($vau ctf env
    (eval
        (select (eval (car ctf) env)
            (car (cdr ctf))
            (car (cdr (cdr ctf))) )
        env )))

(def! "head" (wrap ($vau x #ignore (car . x))))
(def! "tail" (wrap ($vau x env
    #;(print "tail" x)
    (def! "x" (cdr . x))
    (if (combiner? x) (x) x) )))
;    (eval (select (cons? x) (unwrap x) (list (unwrap x))) env)

($defn! head2 xs       (head (tail . xs)))
($defn! tail2 xs       (tail (tail . xs)))
($defn! head3 xs (head (tail (tail . xs))))
#;($defn!  cadr xs      (head (tail . xs)) )
#;($defn!  cddr xs      (tail (tail . xs)) )
#;($defn! caddr xs (head (tail (tail . xs))))

($def! apply (wrap ($vau appv-arg-env static-env
    ($def! appv ( head appv-arg-env))
    ($def! arg  (head2 appv-arg-env))
    ($def! env  (tail2 appv-arg-env))
    (eval (list (unwrap appv) . arg)
        (if (null? env)
            (make-environment static-env)
            env )))))

($defn! cons ad
    ($def! dr (head2 ad))
    (list (head ad) . dr))

;; Concatenate exactly two lists
($defn! concat2 xs-ys
    ($def! xs ( head xs-ys))
    ($def! ys (head2 xs-ys))
    (if (null? xs)
        ys
        (cons (head xs) (concat2 (tail xs) ys)) ))

;; Unpacks values into parameter trees, returning a list of name-value pairs
;; (ns vs) -> ((n . v)*) | #inert
($defn! unpack ns-vs
    ($def! ns ( head ns-vs))
    ($def! vs (head2 ns-vs))
    (if (cons? ns)
        (if (cons? vs)
            (begin
                ($def! left (unpack (head ns) (head vs)))
                (if (eq? left #inert)
                    #inert
                    (begin
                        ($def! right (unpack (tail ns) (tail vs)))
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
    ($def! name   (  head nvt))
    ($def! value  ( head2 nvt))
    ($def! target (head3 nvt))
    ;(apply $def! (list (eval name env) value) ;)
    (eval (list $def! (eval name env) (list (unwrap eval) value env))
        (eval target env))))

($defn! assert cond-msg
    (if (head cond-msg)
        #inert
        (error (head2 cond-msg)) ))

;; Takes a list of name-value pairs and assigns them in an environment
($defn! set-list! nvs-e
    ($def! nvs ( head nvs-e))
    ($def! env (head2 nvs-e))
    (if (null? nvs)
        ()
        (begin
            ($def! nv  (head nvs))
            (set! (car nv) (cdr nv) env)
            (set-list! (tail nvs) env) )))

;; Returns if the pattern matched and assigns the names if it did
($def! match ($vau ns-vs env
    ;; We want to only assign if there's an actual match
    ;; First, depth-first in-order convert into a pair of lists for names and values
    ($def! nvs (unpack (head ns-vs) (eval (head2 ns-vs) env)))
    (if (eq? nvs #inert)
        false  ;; Match failed
        (begin ;; Match succeeded, define everything
            (set-list! nvs env)
            true)) ))

;; We finally have enough machinery to redefine our primitives to be nicer

($def! vau ($vau peb static-env
    ($def! ptree (head peb))
    ($def! penv (head2 peb))
    ($def! body (tail2 peb))
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

(defvau thunk (f) env
    (lambda () (eval f env)))

(defn copy (xs)
    (if (null? xs)
        ()
        (cons (head xs) (copy (tail xs))) ))

($def! and ($vau xs e ;; Returns the first falsey value, or the last true one
    (if (null? xs) true (begin
        ($def! ex (eval (head xs) e))
        ($def! xs       (tail xs)   )
        (if (null? xs)
            ex
            (if ex
                (eval (cons and xs) e)
                ex) )))))
($def! or ($vau xs e ;; Returns the first truthy value, or the last falsey one
    (if (null? xs) false (begin
        ($def! ex (eval (head xs) e))
        ($def! xs       (tail xs)   )
        (if (null? xs) ex
            (if ex ex
                (eval (cons or xs) e) ))))))

(defvau cond patterns env
    (defn go (ps)
        (if (null? ps)
            #inert
            (if (or (eq? (head ps) 'else) (eval (head ps) env))
                (eval (head2 ps) env)
                (go (tail2 ps)) )))
    (go patterns) )

(defn foldl (f b xs)
    (if (null? xs)
        b
        (foldl f (f b (head xs)) (tail xs)) ))

(defn foldr (f b xs)
    (if (null? xs)
        b
        (f (head xs) (foldr f b (tail xs))) ))

(defn map (f xs)
    (if (null? xs)
        ()
        (cons (f (head xs)) (thunk (map f (tail xs)))) ))

(defn any? (p xs)
    (foldl (lambda (acc x) (or acc (p x))) false xs))

(defn all? (p xs)
    (foldl (lambda (acc x) (and acc (p x))) true xs))

(defn zip xs
    (if (any? null? xs)
        ()
        (cons (map head xs) (thunk (apply zip (map tail xs)))) ))

#;(
;; Execute a combiner in a given environment

($defn! list* args
    ($defn! go xs
        ($def! h (head xs))
        ($def! t (tail xs))
        (if (null? t)
            h
            (cons h (go . t)) ))
    (if (null? args)
        (error "arity mismatch: list* got 0 arguments")
        (go . args) ))

#;($defn! apply (appv args)
    (eval (cons (unwrap appv) args) (make-environment)))

($defn! concat xss
    ($defn! cat xs-ys
        ($def! xs ( head xs-ys))
        ($def! ys (head2 xs-ys))
        (if (null? xs) ys
            (cons (head xs) (cat (tail xs) ys))))
    (if (null? xss) ()
        (cat (head xss) (apply concat (tail xss)))))

($def! cond ($vau cs env
    (if (null? cs) () ;; (cond) == ()
        (eval
            (if (eq? (head cs) 'else)
                (head2 cs)
                (if (eval (head cs) env)
                    (head2 cs)
                    (cons cond (tail2 cs)) ))
            env ))))

($defn! map (f xs)
    (if (null? xs) nil
        (cons
            (    f (head xs))
            (map f (tail xs)) )))

($def! $lambda ($vau ptree-body env
    ($def! ptree (head ptree-body))
    ($def! body  (tail ptree-body))
    (wrap (eval (list $vau ptree #ignore . body) env))))

($def! $let ($vau (bindings . body) env
    (id (cons
        (list $lambda (map head bindings) . body)
        (map tail bindings)) env)))

($defn! id (x . #ignore) x)
($defn! const (c) ($lambda #ignore c))
;($defn! zip ())
)