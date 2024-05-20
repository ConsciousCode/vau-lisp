
; Constants which can be derived from intrinsics
(#def nil     ())
(#def true    (eq? nil  nil))
(#def false   (eq? nil true))
(#def #ignore #ignore)

(#def if (#vau (c t f) e (#cond
    ((eval c e) . (eval t e))
    (     true  . (eval f e)) )))
(#def and (#vau xs e
    (if (null? xs) true
        ((wrap (#vau (ex xs) #ignore
            (if (null? xs) ex
                (if ex (eval (cons and xs) e) ex))) )
            (eval (head xs) e)
            (tail xs)) )))
(#def or (#vau xs e
    (if (null? xs) false
        ((wrap (#vau (ex xs) #ignore
            (if (null? xs) ex
                (if ex ex (eval (cons #and xs) e) ))))
            (eval (head xs) e)
            (tail xs)))))

(#def list (wrap (#vau x #ignore x)))
(#def #lambda (#vau (ptree . body) env
    (wrap (eval (list #vau ptree #ignore . body) env))))