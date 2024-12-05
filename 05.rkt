#lang racket

(define (get-rules file)
  (define (read-rule str)
    (map string->number (string-split str "|")))
  (for/list ([line (in-lines file `any)]
             #:break (= (string-length line) 0))
    (read-rule line)))

(define (get-pages file)
  (define (read-pages str)
    (map string->number (string-split str ",")))
  (for/list ([line (in-lines file `any)])
    (read-pages line)))

(define (mid-value pages)
  (list-ref pages (/ (- (length pages) 1) 2)))

(define (all-pairs pages)
  (for*/list ([i (build-list (sub1 (length pages)) values)]
              [j (build-list (sub1 (length pages)) add1)]
              #:when (> j i))
    (list (list-ref pages i) (list-ref pages j))))

(define (breaks-rule pair rule)
    (and (= (cadr pair) (car rule)) (= (car pair) (cadr rule))))

(define (breaks-any-rule pair rules)
  (for/or ([rule rules])
    (breaks-rule pair rule)))

(define (pages-invalid rules pages)
  (for/or ([pair (all-pairs pages)])
    (breaks-any-rule pair rules)))

(define (pair-swapped pages index)
  (append (take pages index) (list (list-ref pages (add1 index)) (list-ref pages index)) (drop pages (+ index 2))))

(define (sort-pages rules pages)
  (define (pass rules pages index)
    (cond
      [(= index (- (length pages) 1)) pages]
      [(breaks-any-rule (list (list-ref pages index) (list-ref pages (add1 index))) rules) (pass rules (pair-swapped pages index) (add1 index))]
      [else (pass rules pages (add1 index))]))
  (define (iter rules pages pass-count)
    (cond
      [(pages-invalid rules pages) (iter rules (pass rules pages 0) (add1 pass-count))]
      [else pages]))
  (iter rules pages 0))

(define (solve01 rules all-pages)
  (for/sum ([pages all-pages]
            #:unless (pages-invalid rules pages))
    (mid-value pages)))

(define (solve02 rules  all-pages)
  (for/sum ([pages all-pages]
            #:when (pages-invalid rules pages))
    (mid-value (sort-pages rules pages))))

(define file (open-input-file "data/05.txt"))
(define rules (get-rules file))
(define all-pages (get-pages file))
(print (solve01 rules all-pages))
