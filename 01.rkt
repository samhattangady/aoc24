#lang racket
;; Now we try this in racket just to see if we have the things
;; working.
;; mostly we don't want to do loop-for that lisp does...

(define (insert vals val)
  (define (iter smaller larger val)
    (cond
      [(empty? larger)
       (reverse (cons val smaller))]
      [(> (car larger) val)
       (append (reverse (cons val smaller)) larger)]
      [else (iter (cons (car larger) smaller) (cdr larger) val)]))
  (iter `() vals val))

(define (insert-vals lists vals)
  (list
    (insert (car lists) (car vals))
    (insert (cadr lists) (cadr vals))))

(define (absdiff v1 v2)
  (cond
    [(> v1 v2) (- v1 v2)]
    [else (- v2 v1)]))

(define (sumdist lists)
  (define (iter lists total)
    (cond
      [(empty? (car lists))
       total]
      [else (iter (list (cdr (car lists)) (cdr (cadr lists))) (+ total (absdiff (car (car lists)) (car (cadr lists)))))]))
  (iter lists 0))

(define (how-many? val vals)
  (define (iter val vals total)
    (cond
      [(empty? vals) total]
      [(= val (car vals))
       (iter val (cdr vals) (+ total 1))]
      [else
        (iter val (cdr vals) total)]))
  (iter val vals 0))

(define (simscore lists)
  (define (score lists)
    (* (car (car lists)) (how-many? (car (car lists)) (cadr lists))))
  (define (iter lists total)
    (cond
      [(empty? (car lists)) total]
      [else
        (iter (list (cdr (car lists)) (cadr lists)) (+ total (score lists)))]))
  (iter lists 0))

(define (solve-p01 filename)
  (define in (open-input-file filename))
  (define (get-lists lists)
    (define line (read-line in `any))
    (cond
      [(eof-object? line)
       lists]
      [else
        (get-lists (insert-vals lists (map string->number (string-split line))))]))
  (define lists (get-lists (list `() `())))
  (println (sumdist lists)))

(define (solve-p02 filename)
  (define in (open-input-file filename))
  (define (get-lists lists)
    (define line (read-line in `any))
    (cond
      [(eof-object? line)
       lists]
      [else
        (get-lists (insert-vals lists (map string->number (string-split line))))]))
  (define lists (get-lists (list `() `())))
  (println (simscore lists)))

(solve-p02 "01.txt")
