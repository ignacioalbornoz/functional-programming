#lang play

#| Whiteboard policy |#
;; En esta tarea se discutió la PARTE E (función sequest) con Kenia Castillo 


#| PARTE A |#

#|
<TaskSchedule> ::= (task <string> <integer>)
                |  (seq-tasks <task> <task>)
                |  (par-tasks <task> <task>)
|#
;; Crea una estructura recursiva de tareas dadas por nombre y tiempo
;; Estas tareas se relacionan con una o más tareas de manera secuencial o paralela 
(deftype TaskSchedule
  (task name duration)
  (seq-tasks t1 t2)
  (par-tasks t1 t2)
  )

#| PARTE B |#
;; is-in :: TaskSchedule string -> bool
;; Retorna si el task schedule dado contiene o no una tarea con el nombre dado por el string
(define (is-in task-schedule str)
(match task-schedule
  [(task name duration) (equal? name str)]
  [(seq-tasks t1 t2) (or (is-in t1 str) (is-in t2 str))]
  [(par-tasks t1 t2) (or (is-in t1 str) (is-in t2 str))]))


#| PARTE C |#
;; length :: TaskSchedule -> integer
;; Devuelve la duración total del task schedule
(define (length task-schedule)
(match task-schedule
  [(task name duration) duration]
  [(seq-tasks t1 t2) (+ (length t1) (length t2))]
  [(par-tasks t1 t2) (max (length t1) (length t2))]))

#| PARTE D |#
;; longest :: TaskSchedule -> cons string integer
;; Devuelve el par nombre y duración de la tarea más larga de un task schedule
(define (longest task-schedule)
(match task-schedule
  [(task name duration) (cons name duration)]
  [(seq-tasks t1 t2) (cond [(> (cdr (longest t1)) (cdr (longest t2))) (longest t1)]
                           [else (longest t2)])]
  [(par-tasks t1 t2) (cond [(> (cdr (longest t1)) (cdr (longest t2))) (longest t1)]
                           [else (longest t2)])]))

#| PARTE E | Función auxiliar |#
;; task-schedule-to-list :: TaskSchedule -> list
;; Convierte un task schedule a una lista donde "t" representa a las tareas y "h" todas las posibles separaciones que genera el tener un par paralelo
(define (task-schedule-to-list task-schedule)
  (match task-schedule
    [(task _ _) (list "t")]
    [(seq-tasks t1 t2) (append (task-schedule-to-list t1) (task-schedule-to-list t2))]
    [(par-tasks t1 t2) (append (list "h") (task-schedule-to-list t1) (list "h") (task-schedule-to-list t2) (list "h"))]
    ))


#| PARTE E | Función auxiliar  |#
;; max-consecutive-seq :: list integer integer -> integer
;; Dada una lista compuesta de "t" y "h" (como la generada por task-schedule-to-list) devuelve la cantidad máxima de "t" seguidas contenida en la lista
(define (max-consecutive-seq tasks-list actual-count max-count)
  (match tasks-list
    [(list) max-count]
    [(cons a b) (cond [(equal? a "t") (max-consecutive-seq b (+ actual-count 1) (max max-count (+ actual-count 1)))]
                      [(equal? a "h") (max-consecutive-seq b 0 (max max-count actual-count))])]
    ))


#| PARTE E |#
;; sequest :: TaskSchedule -> integer
;; Devuelve el largo de la secuencia más larga de tareas individuales (que se realizan de manera secuencial) contenida en un task schedule dado, utilizando
;; max-consecutive-seq en la lista generada de aplicar task-schedule-to-list en task-schedule con los contadores en 0
(define (sequest task-schedule)
  (max-consecutive-seq (task-schedule-to-list task-schedule) 0 0))


#| PARTE F |#
;; end-time :: TaskSchedule string -> integer
;; Devuelve el instante de tiempo en el que termina de ejecutarse una tarea (identificada por su nombre), de acuerdo a un task schedule
;; Si el task schedule dado no contiene una tarea con el nombre consultado entonces devuelve un error
(define (end-time task-schedule str)
(match task-schedule
  [(task name duration) (cond [(equal? name str) duration]
                           [else (error "tarea no encontrada")])]
  [(seq-tasks t1 t2) (cond [(is-in t1 str) (end-time t1 str)]
                           [(is-in t2 str) (+ (length t1) (end-time t2 str))]
                           [else (error "tarea no encontrada")])]
  [(par-tasks t1 t2) (cond [(is-in t1 str) (end-time t1 str)]
                           [(is-in t2 str) (end-time t2 str)]
                           [else (error "tarea no encontrada")])]))


#| PARTE G |#
;; fold :: (string integer -> A) (task task -> A) (task task -> A) -> (TaskSchedule -> A)
;; Captura el esquema de recursión estructural sobre TaskSchedule
(define (fold f g h)
  (λ (task-schedule) 
    (match task-schedule
      [(task name duration) (f name duration)]
      [(seq-tasks t1 t2) (g ((fold f g h) t1) ((fold f g h) t2))]
      [(par-tasks t1 t2) (h ((fold f g h) t1) ((fold f g h) t2))])))


#| PARTE H |#

;; is-in2 :: TaskSchedule -> (string -> bool)
;; Retorna una función que recibe un string y retorna si el task schedule dado anteriormente contiene una tarea con el nombre dado por el string
(define (is-in2 task-schedule)
  (λ (str) ((fold
             (λ (x y) (equal? x str))
             (λ (z1 z2) (or z1 z2))
             (λ (z1 z2) (or z1 z2))) task-schedule))
  )

;; length2 :: TaskSchedule -> integer
;; Devuelve la duración total del task schedule
(define length2
   (fold (λ (x y) y)
         (λ (z1 z2) (+ z1  z2))
         (λ (z1 z2) (max z1  z2)))) ;; eliminie el void y complete

;; longest2 :: TaskSchedule -> cons string integer
;; Devuelve el par nombre y duración de la tarea más larga de un task schedule
(define longest2
   (fold (λ (x y) (cons x y))
         (λ (z1 z2) (cond [(> (cdr z1) (cdr z2)) z1]
                           [else z2]))
         (λ (z1 z2) (cond [(> (cdr z1) (cdr z2)) z1]
                           [else z2])))) ;; eliminie el void y complete
