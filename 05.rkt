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
    (breaks-rule pair rule)
  )
)

(define (pages-invalid rules pages)
  (for/or ([pair (all-pairs pages)])
    (breaks-any-rule pair rules)))

(define (pair-swapped pages index)
  (append (take pages index) (list (list-ref pages (add1 index)) (list-ref pages index)) (drop pages (+ index 2))))

(define (make-pair pages i1 i2)
  (list (list-ref pages i1) (list-ref pages i2)))

(define (sort-pages rules pages)
  (for/fold ([pass-pages pages])
    ([pass (build-list (sub1 (length pages)) values)] #:when (pages-invalid rules pass-pages))
    (for/fold ([pgs pass-pages])
      ([index (build-list (sub1 (length pages)) values)])
      (cond
        [(breaks-any-rule (make-pair pgs index (add1 index)) rules) (pair-swapped pgs index)]
        [else pgs]))))

(define (solve01 rules all-pages)
  (for/sum ([pages all-pages]
            #:unless (pages-invalid rules pages))
    (mid-value pages)))

(define (solve02 rules  all-pages)
  (for/sum ([pages all-pages] #:when (pages-invalid rules pages))
    (mid-value (sort-pages rules pages))
  )
)

(define file (open-input-file "data/05.txt"))
(define rules (get-rules file))
(define all-pages (get-pages file))
(print (solve02 rules all-pages))
