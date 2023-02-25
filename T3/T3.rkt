#lang play

#|

   ========================================
                   Tarea 3
   ========================================
   Nombre: Ignacio Albornoz Alfaro
   Rut: 19896094-2

|#


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
;; tipo inductivo para representar expresiones aritméticas,expresiones con
;; condicionales, identificadores, funciones y llamadas a funciones
(deftype Expr
  (num n)
  (add l r)
  (sub l r)
  (if0 c t f)
  (with x ne b)
  (fun id body)
  (id s)
  (app fun-expr arg-expr))


#|
<s-expr> ::= <num>
           | <sym>
           | (list '+  <s-expr> <s-expr>)
           | (list '-  <s-expr> <s-expr>)
           | (list 'if0  <s-expr> <s-expr> <s-expr>)
           | (list 'fun (list <sym>) <s-expr>)
           | (list <s-expr> <s-expr>)
           | (list 'with (list <sym> <s-expr>) <s-expr>)
|#
;; expresiones s utilizadas como sintaxis concreta para escribir expresiones aritméticas,
;; expresiones con condicionales, identificadores, funciones y llamadas a funciones


;; parse :: s-expr -> Expr
;; convierte s-expressiones en Exprs
(define (parse s-expr)
  (match s-expr
    [ n #:when (number? n) (num n) ]
    [ x #:when (symbol? x) (id x) ]
    [(list '+ l r) (add (parse l) (parse r))]
    [(list '- l r) (sub (parse l) (parse r))]
    [(list 'if0 c t f) (if0 (parse c) (parse t) (parse f))]
    [(list 'fun (list x) b) (fun x (parse b))]
    [(list f a) (app (parse f) (parse a))]    
    [(list 'with (list x e) b) #:when (symbol? x)
         (with x (parse e) (parse b))]))




;; --------- [ Parte 1 ] ---------

;; const? :: Expr -> bool
;; indica si es que la expresión entregada consiste en sólo constantes numéricas, posiblemente combinadas
;; a través de operadores aritméticos y/o condicionales y/o definiciones locales
(define (const? expr)
  (match expr
  [(num n) #t]
  [(add l r) (and (const? l)(const? r))]
  [(sub l r) (and (const? l)(const? r))]
  [(if0 c t f) (and (const? c) (const? t) (const? f))]
  [(with x ne b) (and (const? ne) (const? b))]
  [(fun id body) #f]
  [(id s) #f]
  [(app fun-expr arg-expr) #f]))



;; fold-consts :: Expr -> Expr
;; aplica constant-folding recursivamente, es decir, reconocer y evaluar expresiones constantes en tiempo
;; de compilación, en lugar de hacerlo en tiempo de ejecución
(define (fold-consts expr)
  (match expr
  [(num n) (num n)]
    
  [(add l r) (cond [(and (const? l) (const? r)) (num (+ (num-n (fold-consts l)) (num-n (fold-consts r))))]
                   [else (add (fold-consts l) (fold-consts r))]
                   )]
    
  [(sub l r) (cond [(and (const? l) (const? r)) (num (- (num-n (fold-consts l)) (num-n (fold-consts r))))]
                   [else (sub (fold-consts l) (fold-consts r))]
                   )]
    
  [(if0 c t f) (cond [(and (const? c) (const? t) (const? f)) (if (equal? 0 (fold-consts c)) (fold-consts t) (fold-consts f))]
                     [else (if0 (fold-consts c) (fold-consts t) (fold-consts f))])]
                   
    
  [(with x ne b) (cond [(and (const? ne) (const? b)) (fold-consts b)]
                     [else (with x (fold-consts ne) (fold-consts b))])]
    
  [(fun id body) (fun id body)]
    
  [(id s) (id s)]
    
  [(app fun-expr arg-expr) (app (fold-consts fun-expr) (fold-consts arg-expr))]))






;; --------- [ Parte 2 ] ---------


;; subst :: Expr symbol Expr -> Expr
;; (subst expr sub-id val) sustituye todas las ocurrencias libres del
;; identificador 'sub-id' en la expresión 'expr' por el valor 'val'
(define (subst expr sub-id val)
  (match expr
    [(num n) expr]
    [(add l r) (add (subst l sub-id val)
                    (subst r sub-id val))]
    [(sub l r) (sub (subst l sub-id val)
                    (subst r sub-id val))]
    [(if0 c t f) (if0 (subst c sub-id val)
                      (subst t sub-id val)
                      (subst f sub-id val))]
    [(with bound-id named-expr body) (with bound-id
           (subst named-expr sub-id val)
           (if (symbol=? bound-id sub-id)
               body
               (subst body sub-id val)))]
    [(fun id body) (if (not(equal? id sub-id)) (fun id (subst body sub-id val)) (fun id body))]
    [(id x) (if (symbol=? x sub-id) val expr)]
    [(app f arg-expr) (app (subst f sub-id val)
                           (subst arg-expr sub-id val))]))



;; propagate-consts :: Expr -> Expr
;; propaga las constantes bottom-up, es decir desde las expresiones más internas, hacia la más externa
;; aplica constant propagation recursivamente, es decir, busca reemplazar los identificadores asociados
;; a un valor reducido (en este caso números).
(define (propagate-consts expr)
  (match expr
  [(num n) (num n)]
    
  [(add l r) (add (propagate-consts l) (propagate-consts r))]
    
  [(sub l r) (sub (propagate-consts l) (propagate-consts r))]
    
  [(if0 c t f) (if0 (propagate-consts c) (propagate-consts t) (propagate-consts f))]

  [(with x ne b) (cond [(num? ne) (subst (propagate-consts b) x ne)]                       
                       [else (with x (propagate-consts ne) (propagate-consts b))])]
    
  [(fun id body) (fun id body)]
    
  [(id s) (id s)]
    
  [(app fun-expr arg-expr) (app (propagate-consts fun-expr) (propagate-consts arg-expr))]))



;; --------- [ Parte 3 ] ---------

;; cf&p :: Expr -> Expr
;; aplica constant folding (fold-consts) y propagate-consts (fold-consts) iterativamente hasta que estas no modifiquen más el programa

(define (cf&p expr)
  (cond [(not (equal? (fold-consts expr) expr))  (cf&p(fold-consts expr))]
        [(not (equal? (propagate-consts expr) expr))  (cf&p(propagate-consts expr))]
        [else  expr]))
    

