;; This prelude is an exercise in frugality
;; I wanted to try to implement powerful features on a few very basic primitives
;; The primitives:
;; - def! - definition of one name to one value
;; - eq? - identity comparison of just two values
;; - $vau - binds parameters to a single name, unpacking is manual
;; - wrap - wrap a combiner as an applicative
;; - unwrap - remove an applicative wrapping
;; - eval - evaluate within an environment
;; - car/tail - operators for decomposing pairs
;; - puts/getc - stdin/out

;; Note: #ignore doesn't actually *do* anything until way later when we define
;;  pattern matching. Before that point, it's merely a naming convention.
(def! "quote" ($vau x #ignore (car x)))
(def! "list" (wrap ($vau x #ignore x)))

(def! "bind-line!" (wrap ($vau e-x #ignore
    (setattr! (car (cdr e-x)) "line" (getattr (car e-x) "line")) )))

(def! "$def!" ($vau nv env
    (bind-line! env
        (eval (list def!
            (display (car nv))
            (list (unwrap eval) (car (cdr nv)) env) ) env) )))

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
    (bind-line! env
        (eval (list $def! name (list wrap (list $vau parm #ignore . body))) env) )))

($defn! null? x (eq? nil . x))

($def! if ($vau ctf env
    ($def! tf (cdr ctf))
    (eval
        (select (eval (car ctf) env)
            (car tf)
            (car (cdr tf)) )
        env )))

(def! "head" (wrap ($vau x #ignore (car . x))))
(def! "tail" (wrap ($vau x env
    (def! "x" (cdr . x))
    (if (combiner? x) (x) x) ))) ;; Support for lazy lists

($defn! head2 xs       (head (tail . xs)))
($defn! tail2 xs       (tail (tail . xs)))
($defn! head3 xs (head (tail (tail . xs))))
#;($defn!  cadr xs      (head (tail . xs)) )
#;($defn!  cddr xs      (tail (tail . xs)) )
#;($defn! caddr xs (head (tail (tail . xs))))

($def! apply (wrap ($vau appv-arg-env static-env
    ($def! appv (head  appv-arg-env))
    ($def! arg  (head2 appv-arg-env))
    ($def! env  (tail2 appv-arg-env))
    (eval (list (unwrap appv) . arg)
        (if (null? env)
            (make-environment static-env)
            (head env) )))))

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

($def! when ($vau cb env
    (if (eval (head cb) env)
        (apply begin (tail cb) env)
        #inert )))

($def! unless ($vau cb env
    (if (eval (head cb) env)
        #inert
        (apply begin (tail cb) env) )))

($defn! assert cond-msg
    (unless (head cond-msg)
        (error (head2 cond-msg)) ))

($def! and2 ($vau x-y env
    ($def! x (eval (head x-y) env))
    (if x (eval (head2 x-y) env) x)))

;; Unpacks values into parameter trees, returning a list of name-value pairs
;; (ns vs) -> ((n . v)*) | #inert
;; (case pred?) and (case pred? p) check a predicate and optionally name the result
;; '() special case for nil
;; Quoting evaluates the quoted value and checks if it's true. Doesn't advance value iterator.
($def! unpack (wrap ($vau ns-vs env
    ($def! ns ( head ns-vs))
    ($def! vs (head2 ns-vs))
    (if (cons? ns)
        (begin
            ($def! ns-head (head ns))
            ($def! ns-tail (tail ns))
            (if (eq? ns-head 'case)
                ;; Predicate eg (case empty? x)
                (when ((eval (head ns-tail) env) vs)
                    (if (null? (tail ns-tail))
                        () ;; Unnamed
                        (list (cons (head (tail ns-tail)) vs)) )) ;; Named
                (if (and2 (cons? ns-head) (eq? (head ns-head) 'quote))
                    (if (null? (tail ns-head))
                        (when (null? vs) ())
                        (when (eval (head2 ns-head) env)
                            (unpack ns-tail vs) ))
                    ;; List/cons
                    (when (cons? vs)
                        ($def! left (unpack ns-head (head vs)))
                        (unless (eq? left #inert)
                            ($def! right (unpack ns-tail (tail vs)))
                            (unless (eq? right #inert)
                                (concat2 left right) ))))))
        (if (symbol? ns)
            ;; Symbol
            (if (eq? ns #ignore)
                ()
                (list (cons ns vs)) )
            ;; Constant
            (when (eq? ns vs) ()) )))))

;; Set a name to a value in an environment
;; (this is very hard to inline correctly)
($def! set! ($vau nvt env
    ($def! name   (  head nvt))
    ($def! value  ( head2 nvt))
    ($def! target (head3 nvt))
    ;(apply $def! (list (eval name env) value) ;)
    (eval (list $def! (eval name env) (list (unwrap eval) value env))
        (eval target env))))

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
            true ))))

;; We finally have enough machinery to redefine our primitives to be nicer

($def! vau ($vau peb static-env
    (unless (match (ptree penv . body) peb)
        (error "Pattern match failed:" (display (ptree))))
    ($vau args env
        ($def! local-env (make-environment static-env))
        (if (apply match (list ptree (unwrap args)) local-env)
            (begin
                (set! penv env local-env)
                (eval (cons begin body) local-env))
            (error "Pattern matching failed:" (display ptree) "got" (display args)) ))))

;; Now we have parameter unpacking
($def! lambda (vau (ptree . body) env
    (wrap (apply vau (list ptree #ignore . body) env)) ))

($def! defvau (vau (name . rest) env
    (bind-line! env
        (eval (list $def! name (list vau . rest)) env) )))

($def! defn (vau (name ptree . body) env
    (bind-line! env
        (eval (list $def! name (list lambda ptree . body)) env) )))

($def! def (vau (ptree values) env
    (unless (eval (list match ptree values) env)
        (error "Pattern matching failed:" (display ptree)) )))

(defn unfold (p h t x)
    (if (p x)
        ()
        (cons (h x) (unfold p h t (t x))) ))

(defn int->string ((case int? x))
    (defn go (n acc)
        (if (> n 0)
            (let (((n d) (divmod n 10)))
                (go n (cons (+ d (car "0")) acc)) )
            acc ))
    (go x ()) )

(defn list-cons (xs)
    (if (cons? xs)
        (cons xs (list-cons (tail xs)))
        xs ))

(defn stream (yield . xs)
    (which xs
        () #inert
        (x . xs) (begin
            (yield "(")
            (stream yield x)
            (each x (list-cons xs)
                (yield " ")
                (stream yield (head x)) )
            (unless (null? x)
                (yield " . ")
                (stream yield x) )
            (yield ")") )
        (case bool?) (yield (if xs "true" "false"))
        (case int?) (yield (int->string xs))
        (case symbol?) (yield (display xs))
        (case applicative?) (yield "<applicative>")
        (case combiner?) (yield "<$vau>")
        else (yield "<unknown>") ))

(defvau thunk fs env
    (eval (list begin . fs) env))

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

(defvau let (nv . body) env
    (def local (make-environment env))
    (defn go (xs)
        (if (null? xs)
            #inert
            (begin
                (apply def (head xs) local)
                (go (tail xs)) )))
    (go nv)
    (eval (cons begin body) local) )

(defvau each (var iter . body) env
    (defn go (xs)
        (if (null? xs)
            #inert
            (begin
                (apply def (list var (unwrap (head xs))) env)
                (eval (cons begin body) env)
                (go (tail xs)) )))
    (go (eval iter env)) )

(defvau which (value . cs) env
    (def value (eval value env))
    (defn go cs
        (if (null? cs)
            #inert
            (begin
                (def (cond case . cs) cs)
                (if (or (eq? case 'else) (match cond value))
                    (eval case env)
                    (go . cs) ))))
    (go . cs))

(defn copy (xs) ;; Unlazy a list
    (if (null? xs)
        ()
        (cons (head xs) (copy (tail xs))) ))

(defvau and xs e ;; Returns the first falsey value, or the last true one
    (if (null? xs) true (begin
        ($def! ex (eval (head xs) e))
        ($def! xs       (tail xs)   )
        (if (null? xs)
            ex
            (if ex
                (eval (cons and xs) e)
                ex) ))))
(defvau or xs e ;; Returns the first truthy value, or the last falsey one
    (if (null? xs) false (begin
        ($def! ex (eval (head xs) e))
        ($def! xs       (tail xs)   )
        (if (null? xs) ex
            (if ex ex
                (eval (cons or xs) e) )))))

(defvau cond patterns env
    (defn go (ps)
        (if (null? ps)
            #inert
            (if (or (eq? (head ps) 'else) (eval (head ps) env))
                (eval (head2 ps) env)
                (go (tail2 ps)) )))
    (go patterns) )

(defn any? (p xs)
    (foldl (lambda (acc x) (or acc (p x))) false xs))

(defn all? (p xs)
    (foldl (lambda (acc x) (and acc (p x))) true xs))

(defn zip xs
    (if (any? null? xs)
        ()
        (cons (map head xs) (thunk (apply zip (map tail xs)))) ))

(defn list* args
    (defn go (h . t)
        (if (null? t)
            h
            (cons h (go . t)) ))
    (if (null? args)
        (error "arity mismatch: list* got 0 arguments")
        (go . args) ))

(defn concat xss (foldl concat2 () xss))

(defn inc (x) (+ 1 x))
(defn id (x . #ignore) x)
(defn const (c) (lambda #ignore c))

;; Ok now let's make a meta-circular evaluator

(defn mc-interpreter ()
    (rep-loop (mc-make-initial-env)))

(defn rep-loop (env)
    (display "ϝ>> ")
    (write (mc-eval (read) env))
    (print)
    (rep-loop env) )