#lang play
(require "T1.rkt")

(print-only-errors #t)


#| Creación TaskSchedule |#

;; Caso Base
(define my-TaskSchedule-0
  (task "t0" 1))

(define my-TaskSchedule-0-equal
  (task "t0" 1))

(define my-TaskSchedule-0-f
  (task "t1" 1))

(define my-TaskSchedule-0-f-2
  (task "t0" 2))

;; Casos inductivos

(define my-TaskSchedule-1-2
  (seq-tasks (task "t1" 2) (task "t2" 4)))

(define my-TaskSchedule-1-2-3
  (par-tasks my-TaskSchedule-1-2 (task "t3" 3))
  )

(define my-TaskSchedule-1-2-3-equal
  (par-tasks (seq-tasks (task "t1" 2) (task "t2" 4)) (task "t3" 3))
  )

(define my-TaskSchedule-6-7
  (par-tasks (task "t6" 2) (task "t7" 1))
  )

(define my-TaskSchedule-6-7-seq
  (seq-tasks (task "t6" 2) (task "t7" 1))
  )

(define my-TaskSchedule-4-6-7
  (seq-tasks (task "t4" 2) my-TaskSchedule-6-7)
  )

(define my-TaskSchedule-4-6-7-seq
  (seq-tasks (task "t4" 2) my-TaskSchedule-6-7-seq)
  )

(define my-TaskSchedule-4-6-7-5
  (par-tasks my-TaskSchedule-4-6-7 (task "t5" 6))
  )

(define my-TaskSchedule-4-6-7-seq-5
  (par-tasks my-TaskSchedule-4-6-7-seq (task "t5" 6))
  )

;; Estructuras finales

;; my-TaskSchedule-all :: estructura de enunciado
(define my-TaskSchedule-all
  (seq-tasks my-TaskSchedule-1-2-3 my-TaskSchedule-4-6-7-5)
  )

;; my-TaskSchedule-all :: estructura de enunciado con t6;t7
(define my-TaskSchedule-all-6-7-seq
  (seq-tasks my-TaskSchedule-1-2-3 my-TaskSchedule-4-6-7-seq-5)
  )

;; my-TaskSchedule-1-2-3-4-6-7 :: estructura auxiliar para crear my-TaskSchedule-f
(define my-TaskSchedule-1-2-3-4-6-7
  (seq-tasks my-TaskSchedule-1-2-3 my-TaskSchedule-4-6-7)
  )

;; my-TaskSchedule-f :: estructura similar a enunciado creada en otro orden recursivo debería ser distinta a my-TaskSchedule-all

(define my-TaskSchedule-f
  (par-tasks my-TaskSchedule-1-2-3-4-6-7 (task "t5" 6))
  )


#| PARTE A |#

;; Caso base
(test (TaskSchedule? my-TaskSchedule-0) #t)

;;(test (TaskSchedule? (par-tasks "t5" 6)) #f)

;; Constructores de TaskSchedule

(test (task? my-TaskSchedule-0) #t)
(test (seq-tasks? my-TaskSchedule-1-2) #t)
(test (par-tasks? my-TaskSchedule-6-7) #t)

(test (task? my-TaskSchedule-1-2) #f)
(test (seq-tasks? my-TaskSchedule-6-7) #f)
(test (par-tasks? my-TaskSchedule-0) #f)

;; Casos inductivos
(test (TaskSchedule? my-TaskSchedule-all) #t)
(test (TaskSchedule? my-TaskSchedule-f) #t)
(test (TaskSchedule? my-TaskSchedule-1-2-3-4-6-7) #t)

;; Equals
(test (equal? my-TaskSchedule-0 my-TaskSchedule-0-equal) #t)
(test (equal? my-TaskSchedule-1-2-3 my-TaskSchedule-1-2-3-equal) #t)

(test (equal? my-TaskSchedule-0 my-TaskSchedule-0-f) #f)
(test (equal? my-TaskSchedule-0 my-TaskSchedule-0-f-2) #f)
(test (equal? my-TaskSchedule-all my-TaskSchedule-f) #f)
(test (equal? my-TaskSchedule-all my-TaskSchedule-all-6-7-seq) #f)

#| PARTE B |#

;; Casos bases

(test (is-in my-TaskSchedule-0 "t0") #t)
(test (is-in my-TaskSchedule-0 "t1") #f)

;; Casos inductivos

(test (is-in my-TaskSchedule-all "t1") #t)
(test (is-in my-TaskSchedule-all "t4") #t)
(test (is-in my-TaskSchedule-all "t7") #t)

(test (is-in my-TaskSchedule-all " collect ") #f)
(test (is-in my-TaskSchedule-all "collect") #f)


#| PARTE C |#

;; Casos bases
(test (length my-TaskSchedule-0) 1)
(test (length my-TaskSchedule-0-f-2) 2)

;; Casos inductivos
(test (length my-TaskSchedule-4-6-7-seq) 5)
(test (length my-TaskSchedule-all-6-7-seq) 12)
(test (length my-TaskSchedule-all) 12)


#| PARTE D |#

;; Casos bases

(test (longest my-TaskSchedule-0) (cons "t0" 1))
(test (longest my-TaskSchedule-0-f-2) (cons "t0" 2))
(test (longest my-TaskSchedule-0-f) (cons "t1" 1))

;; Casos inductivos

(test (longest my-TaskSchedule-all) (cons "t5" 6))
(test (longest my-TaskSchedule-all-6-7-seq) (cons "t5" 6))
(test (or (equal? (longest my-TaskSchedule-4-6-7-seq) (cons "t4" 2)) (equal? (longest my-TaskSchedule-4-6-7-seq) (cons "t6" 2))) #t)



#| PARTE E |#

;; Casos bases

(test (task-schedule-to-list my-TaskSchedule-0) (list "t"))
(test (max-consecutive-seq (task-schedule-to-list my-TaskSchedule-0) 0 0) 1)
(test (sequest my-TaskSchedule-0) 1)

;; Casos inductivos

(test (task-schedule-to-list my-TaskSchedule-all) (list "h" "t" "t" "h" "t" "h" "h" "t" "h" "t" "h" "t" "h" "h" "t" "h"))
(test (task-schedule-to-list my-TaskSchedule-all-6-7-seq) (list "h" "t" "t" "h" "t" "h" "h" "t" "t" "t" "h" "t" "h"))

(test (max-consecutive-seq (task-schedule-to-list my-TaskSchedule-all) 0 0) 2)
(test (max-consecutive-seq (task-schedule-to-list my-TaskSchedule-all-6-7-seq) 0 0) 3)

(test (sequest my-TaskSchedule-all) 2)
(test (sequest my-TaskSchedule-all-6-7-seq) 3)


#| PARTE F |#

;; Casos bases
(test (end-time my-TaskSchedule-0 "t0") 1)
(test/exn (end-time my-TaskSchedule-all "t0") "tarea no encontrada")

;; Casos inductivos
(test (end-time my-TaskSchedule-all "t7") 9)
(test (end-time my-TaskSchedule-all-6-7-seq "t7") 11)
(test/exn (end-time my-TaskSchedule-all "t11") "tarea no encontrada")


#| PARTE G |#

;; Función auxiliar

;; number-of-tasks :: TaskSchedule -> integer
;; Devuelve la cantidad de tareas totales de un task schedule
(define (number-of-tasks task-schedule)
  (match task-schedule
    [(task _ _) 1]
    [(seq-tasks t1 t2) (+ (number-of-tasks t1) (number-of-tasks t2))]
    [(par-tasks t1 t2) (+ (number-of-tasks t1) (number-of-tasks t2))]
    ))

(test (number-of-tasks my-TaskSchedule-0) 1)
(test (number-of-tasks my-TaskSchedule-1-2-3) 3)
(test (number-of-tasks my-TaskSchedule-all-6-7-seq) 7)
(test (number-of-tasks my-TaskSchedule-all) 7)

;; Casos bases
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-0) (number-of-tasks my-TaskSchedule-0))
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-0) 1)

;; Casos inductivos
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-1-2-3) (number-of-tasks my-TaskSchedule-1-2-3))
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-1-2-3) 3)
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-all-6-7-seq) (number-of-tasks my-TaskSchedule-all-6-7-seq))
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-all-6-7-seq) 7)
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-all) (number-of-tasks my-TaskSchedule-all))
(test ((fold (λ (x y) 1) + +) my-TaskSchedule-all) 7)


#| PARTE H |#

;; TEST :: is-in2

;; Casos bases

(test ((is-in2 my-TaskSchedule-0) "t0") #t)
(test ((is-in2 my-TaskSchedule-0) "t1") #f)

;; Casos inductivos

(test ((is-in2 my-TaskSchedule-all) "t1") #t)
(test ((is-in2 my-TaskSchedule-all) "t4") #t)
(test ((is-in2 my-TaskSchedule-all) "t7") #t)

(test ((is-in2 my-TaskSchedule-all) " collect ") #f)
(test ((is-in2 my-TaskSchedule-all) "collect") #f)

;; TEST :: length2

;; Casos bases
(test (length2 my-TaskSchedule-0) 1)
(test (length2 my-TaskSchedule-0-f-2) 2)

;; Casos inductivos
(test (length2 my-TaskSchedule-4-6-7-seq) 5)
(test (length2 my-TaskSchedule-all-6-7-seq) 12)
(test (length2 my-TaskSchedule-all) 12)


;; TEST :: longest2

;; Casos bases

(test (longest2 my-TaskSchedule-0) (cons "t0" 1))
(test (longest2 my-TaskSchedule-0-f-2) (cons "t0" 2))
(test (longest2 my-TaskSchedule-0-f) (cons "t1" 1))

;; Casos inductivos

(test (longest2 my-TaskSchedule-all) (cons "t5" 6))
(test (longest2 my-TaskSchedule-all-6-7-seq) (cons "t5" 6))
(test (or (equal? (longest2 my-TaskSchedule-4-6-7-seq) (cons "t4" 2)) (equal? (longest2 my-TaskSchedule-4-6-7-seq) (cons "t6" 2))) #t)
