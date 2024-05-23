;; This prelude is an exercise in frugality
;; I wanted to try to implement powerful features on a few very basic primitives
;; The primitives:
;; - def! - definition of one name to one value
;; - eq? - identity comparison of just two values
;; - $vau - binds parameters to a single name, unpacking is manual
;; - wrap - wrap a combiner as an applicative
;; - unwrap - remove an applicative wrapping
;; - eval - evaluate within an environment
;; - car/cdr - operators for decomposing pairs
;; - puts/gets - stdin/out

(print "Begin")

;; Note: #ignore doesn't actually *do* anything until way later when we define
;;  pattern matching. Before that point, it's merely a naming convention.
;; As an optimization detail, the interpreter *does* respect #ignore
(def! "list" (wrap ($vau x #ignore x)))

(def! "bind-line!" (wrap ($vau e-x #ignore
    (setattr! (car (cdr e-x)) "line" (getattr (car e-x) "line")) )))

;; Low-level %if which doesn't respect lazy lists. Generally faster but
;;  doesn't compose well and doesn't validate its parameters
(def! "%if" ($vau ctf env
    (def! "tf" (cdr ctf))
    (eval
        (select (eval (car ctf) env)
            (car tf)
            (car (cdr tf)) )
        env )))

(def! "%apply" (wrap ($vau appv-arg-env static-env
    (def! "arg" (car (cdr appv-arg-env)))
    (def! "env" (cdr (cdr appv-arg-env)))
    (eval (list (unwrap (car appv-arg-env)) . arg)
        (car (cdr (cdr appv-arg-env))) ))))

(def! "apply" (wrap ($vau appv-arg-env static-env
    (def! "arg" (car (cdr appv-arg-env)))
    (def! "env" (cdr (cdr appv-arg-env)))
    (eval (list (unwrap (car appv-arg-env)) . arg)
        (%if (null? env)
            (make-environment static-env)
            (car env) )))))

(def! "$def!" ($vau nv env
    (bind-line! env
        (%apply def! (list (display (car nv)) (eval (car (cdr nv)) env)) env) )))

(print "Define definition")

($def! quote ($vau x #ignore (car x)))
($def! #inert ($def! nil ())) ;; REPL displays nothing
($def! true  (eq? nil  nil))
($def! false (eq? true nil))
($def! #ignore '#ignore)
($def! $current-environment ($vau #ignore e e))

($def! len= (wrap ($vau xs-len #ignore
    ($def! xs       (car xs-len) )
    ($def! len (car (cdr xs-len)))
    (%if (null? xs)
        (== len 0)
        (len= (cdr xs) (- len 1)) ))))

($def! when ($vau cb env
    (%if (eval (head cb) env)
        (%apply begin (tail cb) env)
        #inert )))

($def! unless ($vau cb env
    (%if (eval (head cb) env)
        #inert
        (%apply begin (tail cb) env) )))

($def! combiner? (wrap ($vau x #ignore
    (%if (applicative? . x)
        true
        (operative? . x) ))))

($def! head (wrap ($vau x #ignore (car . x))))
($def! tail (wrap ($vau x env
    ($def! x (cdr . x))
    (%if (combiner? x) (x) x) ))) ;; Support for lazy lists

($def! head2 (wrap ($vau xs #ignore       (head (tail . xs))) ))
($def! tail2 (wrap ($vau xs #ignore       (tail (tail . xs))) ))
($def! head3 (wrap ($vau xs #ignore (head (tail (tail . xs))))))

($def! if ($vau ctf env
    (unless (len= ctf 3)
        (error "if got" (length ctf) "arguments") )
    (%apply %if ctf env) ))

($def! begin ($vau body env
    ((%apply $vau (list #ignore #ignore . body) env)) ))

;; Set a name to a value in an environment
($def! set! (wrap ($vau nvt #ignore
    (%apply def! (list (display (head nvt)) (head2 nvt)) (head3 nvt)) )))

($def! $defn! ($vau npb env
    ($def! pb   (cdr npb))
    ($def! body (cdr  pb))
    ($def! fn (%apply $vau (list (car pb) #ignore . body) env))
    (set! (car npb) (wrap fn) env)
    (bind-line! env fn)
    #inert ))

($def! assert ($vau cond-msg env
    (unless (head cond-msg)
        (error (eval (head2 cond-msg) env) ))))

($defn! null? x (eq? nil . x))

($defn! cons ad
    ($def! dr (head2 ad))
    (list (head ad) . dr))

;; Concatenate exactly two lists
($defn! concat2 xs-ys
    ($def! xs (head  xs-ys))
    ($def! ys (head2 xs-ys))
    (%if (null? xs)
        ys
        (cons (head xs) (concat2 (tail xs) ys)) ))

($def! and2 ($vau x-y env
    ($def! x (eval (head x-y) env))
    (%if x (eval (head2 x-y) env) x)))

;; Unpacks values into parameter trees, returning a list of name-value pairs
;; (ns vs) -> ((n . v)*) | #inert
;; (case pred?) and (case pred? p) check a predicate and optionally name the result
;; '() special case for nil
;; Quoting evaluates the quoted value and checks if it's true. Doesn't advance value iterator.
($def! unpack (wrap ($vau ns-vs env
    ($def! ns ( head ns-vs))
    ($def! vs (head2 ns-vs))
    (%if (cons? ns)
        (begin
            ($def! ns-head (head ns))
            ($def! ns-tail (tail ns))
            (%if (eq? ns-head 'case)
                ;; Predicate eg (case empty? x)
                (when ((eval (head ns-tail) env) vs)
                    (%if (null? (tail ns-tail))
                        () ;; Unnamed
                        (list (cons (head (tail ns-tail)) vs)) )) ;; Named
                (%if (and2 (cons? ns-head) (eq? (head ns-head) 'quote))
                    (%if (null? (tail ns-head))
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
        (%if (symbol? ns)
            ;; Symbol
            (%if (eq? ns #ignore)
                ()
                (list (cons ns vs)) )
            ;; Constant
            (when (eq? ns vs) ()) )))))

;; Takes a list of name-value pairs and assigns them in an environment
($defn! set-list! nvs-e
    ($def! nvs (head  nvs-e))
    ($def! env (head2 nvs-e))
    (%if (null? nvs)
        ()
        (begin
            ($def! nv  (head nvs))
            (set! (car nv) (cdr nv) env)
            (set-list! (tail nvs) env) )))

;; Returns if the pattern $matched and assigns the names if it did
($def! match (wrap ($vau ns-vs env
    ;; We want to only assign if there's an actual $match
    ;; First, depth-first in-order convert into a pair of lists for names and values
    ($def! nvs (%apply unpack (list (head ns-vs) (head2 ns-vs)) env))
    (%if (eq? nvs #inert)
        false  ;; $match failed
        (begin ;; $match succeeded, define everything
            (set-list! nvs env)
            true )))))

($def! $match ($vau ns-vs env
    (%apply match (list (head ns-vs) (eval (head2 ns-vs) env)) env) ))

(print "Pattern matching")

;; We finally have enough machinery to redefine our primitives to be nicer

($def! vau ($vau peb static-env
    (unless ($match (ptree penv . body) peb)
        (error "Pattern match failed:" (display (ptree))))
    ($vau args env
        ($def! local-env (make-environment static-env))
        (%if (%apply $match (list ptree (unwrap args)) local-env)
            (begin
                (set! penv env local-env)
                (eval (cons begin body) local-env))
            (error "Pattern match failed:" (display ptree) "got" (display args)) ))))

;; Now we have parameter unpacking
($def! lambda (vau (ptree . body) env
    (wrap (%apply vau (list ptree #ignore . body) env)) ))

($def! defvau (vau (name . rest) env
    (bind-line! env
        (eval (list $def! name (list vau . rest)) env) )))

($def! defn (vau (name ptree . body) env
    (bind-line! env
        (eval (list $def! name (list lambda ptree . body)) env) )))

($def! def (vau (ptree values) env
    (unless (%apply $match (list ptree values) env)
        (error "Pattern matching failed:" (display ptree)) )))

(print "Higher-order forms")

(defn unfold (p h t x)
    (%if (p x)
        ()
        (cons (h x) (unfold p h t (t x))) ))

(defn int->string ((case int? x))
    (defn go (n acc)
        (%if (> n 0)
            (let (((n d) (divmod n 10)))
                (go n (cons (+ d (car "0")) acc)) )
            acc ))
    (go x ()) )

(defn list-cons (xs)
    (%if (cons? xs)
        (cons xs (list-cons (tail xs)))
        xs ))

(defn stream (yield . xs)
    (print "stream" xs (typeof xs))
    (each v xs
        (print "stream" v)
        (which v
            () (print "empty?")
            (x . xs) (begin
                (print "hello")
                (yield "(")
                (stream yield x)
                (each x (list-cons xs)
                    (yield " ")
                    (stream yield (head x)) )
                (unless (null? x)
                    (yield " . ")
                    (stream yield x) )
                (yield ")") )
            (case bool?) (yield (%if v "true" "false"))
            (case int?) (begin (print "int") (yield (int->string v)))
            (case symbol?) (yield (display v))
            (case applicative?) (yield "<applicative>")
            (case combiner?) (yield "<$vau>")
            else (yield "<unknown>") )))

(defvau thunk fs env
    (eval (list begin . fs) env))

(defn foldl (f b xs)
    (%if (null? xs)
        b
        (foldl f (f b (head xs)) (tail xs)) ))

(defn foldr (f b xs)
    (%if (null? xs)
        b
        (f (head xs) (foldr f b (tail xs))) ))

(defn map (f xs)
    (%if (null? xs)
        ()
        (cons (f (head xs)) (thunk (map f (tail xs)))) ))

(defvau let (nv . body) env
    (def local (make-environment env))
    (defn go (xs)
        (%if (null? xs)
            #inert
            (begin
                (%apply def (head xs) local)
                (go (tail xs)) )))
    (go nv)
    (eval (cons begin body) local) )

(defvau each (var iter . body) env
    (defn go (xs)
        (%if (null? xs)
            #inert
            (begin
                (%apply def (list var (unwrap (head xs))) env)
                (eval (cons begin body) env)
                (go (tail xs)) )))
    (go (eval iter env)) )

(defvau which (value . cs) env
    (def value (eval value env))
    ($defn! go cs
        (%if (null? cs)
            #inert
            (begin
                (def (cond c . cs) cs)
                (%if (or (eq? cond 'else) (%apply match (list cond value) env))
                    (eval c env)
                    (go . cs) ))))
    (go . cs))

(print "Higher-order functions")

(defn copy (xs) ;; Unlazy a list
    (%if (null? xs)
        ()
        (cons (head xs) (copy (tail xs))) ))

(defvau and xs e ;; Returns the first falsey value, or the last true one
    (%if (null? xs) true (begin
        ($def! ex (eval (head xs) e))
        ($def! xs       (tail xs)   )
        (%if (null? xs)
            ex
            (%if ex
                (eval (cons and xs) e)
                ex) ))))
(defvau or xs e ;; Returns the first truthy value, or the last falsey one
    (%if (null? xs) false (begin
        ($def! ex (eval (head xs) e))
        ($def! xs       (tail xs)   )
        (%if (null? xs) ex
            (%if ex ex
                (eval (cons or xs) e) )))))

(defvau cond patterns env
    (defn go (ps)
        (%if (null? ps)
            #inert
            (%if (or (eq? (head ps) 'else) (eval (head ps) env))
                (eval (head2 ps) env)
                (go (tail2 ps)) )))
    (go patterns) )

(defn any? (p xs)
    (foldl (lambda (acc x) (or acc (p x))) false xs))

(defn all? (p xs)
    (foldl (lambda (acc x) (and acc (p x))) true xs))

(defn zip xs
    (%if (any? null? xs)
        ()
        (cons (map head xs) (thunk (%apply zip (map tail xs)))) ))

(defn list* args
    (defn go (h . t)
        (%if (null? t)
            h
            (cons h (go . t)) ))
    (%if (null? args)
        (error "arity mis$match: list* got 0 arguments")
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

(print "Done")