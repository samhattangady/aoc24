#lang racket

(define (parse-operation line)
  (define ops (string-split line " "))
  (cons (string->number (string-trim (car ops) ":")) (map string->number (cdr ops))))

(define (get-opcombs all-ops num)
  (cond
    [(= num 1) 
     (for/list ([op all-ops])
               (list op))]
    [else
      (define prev (get-opcombs all-ops (sub1 num)))
      (for/fold 
          ([all-combs `()])
          ([pr prev])
          (append 
            (for/list ([op all-ops]) (cons op pr)) 
            all-combs
          )
      ) ]))

(define (apply-op op val1 val2)
  (cond
    [(equal? op `plus) (+ val1 val2)]
    [(equal? op `into) (* val1 val2)]
    [(equal? op `appe) (string->number (string-append (number->string val1) (number->string val2)))]
    [else 0]))

(define (apply-ops ops vals)
  (for/fold ([total (car vals)])
            ([op ops]
             [val (cdr vals)])
            (apply-op op total val)))

(define (valid-operation all-ops single-operation)
  (define total (car single-operation))
  (define vals (cdr single-operation))
  (define op-combs (get-opcombs all-ops (sub1 (length vals))))
  (for/first ([op op-combs] 
              #:when (= (apply-ops op vals) total))
              total
  )
)

(define (to-number val)
  (if (number? val) val 0))

(define (solve01 port)
  (for/sum ([line (in-lines port `any)])
           (to-number (valid-operation `(plus into) (parse-operation line)))
  ))

(define (solve02 port)
  (for/sum ([line (in-lines port `any)])
           (to-number (valid-operation `(plus into appe) (parse-operation line)))
  ))

(define file (open-input-file "data/07.txt"))
(print (solve02 file))
