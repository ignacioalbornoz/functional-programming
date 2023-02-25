#lang play
(require "T3.rkt")

(print-only-errors #t)


#|
<expr> ::= (num <num>)
         | (add <expr> <expr>)
         | (sub <expr> <expr>)
         | (if0 <expr> <expr> <expr>)
         | (with <sym> <expr> <expr>)
         | (id <id>)
         | (fun <sym> <expr>)
         | (app <expr> <expr>)
|#

(define my-Expr-num
  (num 1))

(define my-Expr-add
  (add 1 2))

(define my-Expr-if0-t
  (if0 0 1 2))

(define my-Expr-if0-f
  (if0 3 1 4))

(define my-Expr-with
  (with (id 'x) (num 1) (id 'x)))

(define my-Expr-0-f-2
  (fun (id 'x) (num 1)))

(define my-Expr-id
  (id 'x))

(define my-Expr-app
  (app my-Expr-0-f-2 (num 2) ))


(test (Expr? my-Expr-num) #t)
(test (Expr? my-Expr-add) #t)
(test (Expr? my-Expr-if0-t) #t)
(test (Expr? my-Expr-if0-f) #t)
(test (Expr? my-Expr-with) #t)
(test (Expr? my-Expr-0-f-2) #t)
(test (Expr? my-Expr-id) #t)
(test (Expr? my-Expr-app) #t)

(test (Expr? (+ 5 2)) #f)
(test (Expr? (if 5 2 3)) #f)

(test (Expr? (if0 (add (num 1) (num 2)) (add (num 3) (num 15)) (sub (num 4) (num 6)))) #t)
(test (Expr? (with 'x (add (num 6) (num 7)) (with 'y (add (id 'x) (num 17)) (with 'z (add (id 'y) (num 9)) (add (num 3) (id 'z)))))) #t)
(test (Expr? (with 'x (add (num 2) (app (fun 'y (id 'y)) (num 10))) (add (id 'x) (num 8)))) #t)



;; parse :: s-expr -> Expr
(test (equal? (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}}) (if0 (add (num 1) (num 2)) (add (num 3) (num 45)) (sub (num 4) (num 6)))) #t)
(test (equal? (parse '{with {x 18} 3}) (with 'x (num 18) (num 3))) #t)
(test (equal? (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}}) (if0 (add (num 1) (num 2)) (add (num 3) (num 45)) (sub (num 4) (num 6))) )#t)
(test (equal? (parse '{fun {y} {with {x 7} {+ x x}}}) (fun 'y (with 'x (num 7) (add (id 'x) (id 'x)))) )#t)
(test (equal? (parse '{with {x y} {with {y x}{+ x y}}}) (with 'x (id 'y) (with 'y (id 'x) (add (id 'x) (id 'y)))) )#t)
(test (equal? (parse '{with {y 5} {+ {with {y 6} y} y}}) (with 'y (num 5) (add (with 'y (num 6) (id 'y)) (id 'y))) )#t )
(test (equal? (parse '{if0 {+ 0 0} {{fun {x} x} 7} {with {x {- 1 10}} {+ x x}}}) (if0 (add (num 0) (num 0)) (app (fun 'x (id 'x)) (num 7)) (with 'x (sub (num 1) (num 10)) (add (id 'x) (id 'x)))) )#t)


(test (equal? (parse '{+ 1 2}) (num 3)) #f)
(test (equal? (parse '{+ 1 2}) (add 3 4)) #f)



;; --------- [ Parte 1 ] ---------

;; const? :: Expr -> bool

(test (const? (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}})) #t)
(test (const? (parse '{with {x 18} 3})) #t)
(test (const? (parse '{{fun {x} x} 19})) #f)
(test (const? (parse '{fun {x} 10})) #f)
(test (const? (parse '{with {x 5} {+ 1 x}})) #f)
(test (const? (with 'x (id 'y) (num 5))) #f)

(test (const? (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}})) #t)
(test (const? (parse '{if0 {fun {x} 10} {+ 3 45} {- 4 6}})) #f) 

;; fold-consts :: Expr -> Expr

;; if0
(test (fold-consts (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}})) (num -2))
(test (fold-consts (parse '{if0 (+ 3 4) {fun {x} 10} (- 3 4)})) (if0 (num 7) (fun 'x (num 10)) (num -1)))
(test (fold-consts (parse '{if0 {fun {x} 10} {+ 3 45} {- 4 6}})) (if0 (fun 'x (num 10)) (num 48) (num -2)))
(test (fold-consts (parse '{if0 {- 4 5} {with {x 9} {+ {+ 2 4} 4}} {- 7 2}})) (num 5))
(test (fold-consts (parse '{if0 {- 4 5} {with {x 9} {+ {+ x 4} 4}} {- 7 2}})) (if0 (num -1) (with 'x (num 9) (add (add (id 'x) (num 4)) (num 4))) (num 5)))

;; with
(test (fold-consts (parse '{with {x 18} 3})) (num 3))
(test (fold-consts (parse '{with {x {+ 2 3}} {+ 1 x}})) (with 'x (num 5) (add (num 1) (id 'x))))
(test (fold-consts (parse '{with {y {fun {x} 10}} 2})) (with 'y (fun 'x (num 10)) (num 2)))
(test (fold-consts (parse '{with {x 3} {if0 {+ 3 1} {+ 2 7} {- {with {y 2} {+ 3 2}} 4}}})) (num 1))
(test (fold-consts (parse '{with {z {+ 3 4}} z})) (with 'z (num 7) (id 'z)))
(test (fold-consts (parse '{with {x 5} {+ 1 x}})) (with 'x (num 5) (add (num 1) (id 'x))))

;; fun
(test (fold-consts (parse '{fun {x} 10})) (fun 'x (num 10)))
(test (fold-consts (parse '{{fun {x} x} 19})) (app (fun 'x (id 'x)) (num 19)))
(test (fold-consts (parse '{{fun {x} {+ 2 3}} 7}))(app (fun 'x (add (num 2) (num 3))) (num 7)))
(test (fold-consts (parse '{{fun {y} {+ 7 7}} {+ 2 2}})) (app (fun 'y (add (num 7) (num 7))) (num 4)))


;; --------- [ Parte 2 ] ---------

;; subst :: Expr symbol Expr -> Expr

(test (subst (parse '(+ x 5)) 'x 3)(add 3 (num 5)))
(test (subst (parse '(if0 x 5 (- 2 1))) 'x 1) (if0 1 (num 5) (sub (num 2) (num 1))))
(test (subst (parse '{with {x 23} (+ x 2)}) 'x 1) (with 'x (num 23) (add (id 'x) (num 2))))

(test (subst (parse '{with {x 23} {fun {y} x}})'x 1) (with 'x (num 23) (fun 'y (id 'x))))
(test (subst (parse '{with {x 3} {with {y x}{+ x y}}}) 'x 2) (with 'x (num 3) (with 'y (id 'x) (add (id 'x) (id 'y)))))
(test (subst (parse '{fun {y} {with {x 7} {+ x x}}}) 'x 5) (fun 'y (with 'x (num 7) (add (id 'x) (id 'x)))))

(test (subst (parse '{with {x 23} {fun {y} x}})'y 5) (with 'x (num 23) (fun 'y (id 'x))))
(test (subst (parse '{with {x 3} {with {y x}{+ x y}}}) 'y 4) (with 'x (num 3) (with 'y (id 'x) (add (id 'x) (id 'y)))))
(test (subst (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}}) 'y 3) (if0 (add (num 1) (num 2)) (add (num 3) (num 45)) (sub (num 4) (num 6))))

;; propagate-consts :: Expr -> Expr

;; enunciado
(test (propagate-consts (parse '{with {x 3} {with {y x}{+ x y}}})) (with 'y (num 3)(add (num 3)(id 'y))))
(test (propagate-consts (parse '{if0 {+ 1 2} {+ 3 45} {- 4 6}}))(if0 (add (num 1) (num 2)) (add (num 3) (num 45)) (sub (num 4) (num 6))))
(test (propagate-consts (parse '{{fun {x} x} 7}))(app (fun 'x (id 'x)) (num 7)))
(test (propagate-consts (parse '{with {x 23} {fun {y} x}}))(fun 'y (num 23)))
(test (propagate-consts (parse '{fun {y} {with {x 7} {+ x x}}})) (fun 'y (with 'x (num 7) (add (id 'x) (id 'x)))))


;; with
(test (propagate-consts (parse '{with {x y} {with {y x}{+ x y}}})) (with 'x (id 'y) (with 'y (id 'x) (add (id 'x) (id 'y)))))
(test (propagate-consts (parse '{with {y 5} {+ {with {y 6} y} y}})) (add (num 6) (num 5)) )
(test (propagate-consts (parse '{with {x {+ 3 2}} {+ x 4}})) (with 'x (add (num 3) (num 2)) (add (id 'x) (num 4))))
(test (propagate-consts (parse '{with {x {with {y 5} y}} {+ 1 x}})) (with 'x (num 5) (add (num 1) (id 'x))))
(test (propagate-consts (parse '{with {x {with {x 3} x}} x})) (with 'x (num 3) (id 'x)))
(test (propagate-consts (parse '{with {x {+ 2 3}} x})) (with 'x (add (num 2) (num 3)) (id 'x)))
(test (propagate-consts (parse '{with {x 3} {with {x {+ 2 3}} x}})) (with 'x (add (num 2) (num 3)) (id 'x)))
(test (propagate-consts (parse '{with {x 3} {with {y x}{+ x y}}})) (with 'y (num 3)(add (num 3)(id 'y))))
(test (propagate-consts (parse '{with {x 4} {fun {y} x}}))(fun 'y (num 4)))

;; if0
(test (propagate-consts (parse '{if0 {+ 1 2} {+ 3 45} {- 10 20}}))(if0 (add (num 1) (num 2)) (add (num 3) (num 45)) (sub (num 10) (num 20))))

;; fun
(test (propagate-consts (parse '{fun {y} {with {x 18} {+ x x}}})) (fun 'y (with 'x (num 18) (add (id 'x) (id 'x)))))
(test (propagate-consts (parse '{fun {y} {with {x 7} {+ x {fun {z} x}}}})) (fun 'y (with 'x (num 7) (add (id 'x) (fun 'z (id 'x))))))

;; app
(test (propagate-consts (parse '{{fun {x} x} 32}))(app (fun 'x (id 'x)) (num 32)))



;; --------- [ Parte 3 ] ---------

;; cf&p :: Expr -> Expr

;;enunciado
(test (cf&p (parse '{if0 {+ 1 2} {+ 3 15} {- 4 6}})) (num -2))
(test (cf&p (parse '{with {x {+ 6 7}}{with {y {+ x 17}}{with {z {+ y 9}}{+ 3 z}}}}))(num 42))
(test (cf&p (parse '{{fun {x} x} 7}))(app (fun 'x (id 'x)) (num 7)))
(test (cf&p (parse '{with {x {+ 2 {{fun {y} y} 10}}} {+ x 8}}))(with 'x (add (num 2) (app (fun 'y (id 'y)) (num 10))) (add (id 'x) (num 8))))

;; with
(test (cf&p (parse '{with {y 2} {with {z 3} {with {y 4} {+ y z}}}})) (num 7))

;; fun
(test (cf&p (parse '{with {x {+ 2 {{fun {y} y} 10}}} {+ x 8}})) (parse '{with {x {+ 2 {{fun {y} y} 10}}} {+ x 8}}))

;; app
(test (cf&p (parse '{if0 {+ 0 0} {{fun {x} x} 7} {with {x {- 1 10}} {+ x x}}})) (if0 (num 0) (app (fun 'x (id 'x)) (num 7)) (num -18)))


