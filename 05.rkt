#lang racket
(define (++ val) (+ val 1))

(define (get-rules-and-pages filename)
  (define (read-rule str)
    (map string->number (string-split str "|")))
  (define (read-pages str)
    (map string->number (string-split str ",")))
  (define (add-rule rp rule)
    (list (cons rule (car rp)) (cadr rp)))
  (define (add-pages rp pages)
    (list (car rp) (cons pages (cadr rp))))
  (define in (open-input-file filename))
  (define (iter rp reading-rules)
    (define line (read-line in `any))
    (cond
      [(eof-object? line) (list (reverse (car rp)) (reverse (cadr rp)))]
      [(= (string-length line) 0) (iter rp false)]
      [reading-rules (iter (add-rule rp (read-rule line)) true)]
      [else (iter (add-pages rp (read-pages line)) false) ])
    )
  (iter `(() ()) true))

(define (mid-value pages)
  (list-ref pages (/ (- (length pages) 1) 2)))

(define (all-pairs pages)
  (define (iter pairs pages start other)
    (cond
      [(= start (length pages)) pairs]
      [(= other (length pages)) (iter pairs pages (++ start) (++ (++ start)))]
      [else (iter (cons (list (list-ref pages start) (list-ref pages other)) pairs) pages start (++ other))]))
  (iter `() pages 0 1))

(define (breaks-rule pair rule)
  (cond
    [(and (= (cadr pair) (car rule)) (= (car pair) (cadr rule))) true]
    [else false]))
(define (breaks-any-rule pair rules)
  (cond
    [(empty? rules) false]
    [(breaks-rule pair (car rules)) true]
    [else (breaks-any-rule pair (cdr rules))]))

(define (pages-invalid rules pages)
  (ormap (lambda (pair) (breaks-any-rule pair rules)) (all-pairs pages)))

(define (add-values rule vals)
  (set-add (set-add vals (car rule)) (cadr rule)))
(define (get-all rules)
  (define (iter rules vals)
    (cond
      [(empty? rules) vals]
      [else (iter (cdr rules) (add-values (car rules) vals))]))
  (iter rules (list->set `())))

(define (complete-order rules)

  ;; gets the value that is not first in any of the remaining rules
  (define (get-last rules)
    (define (iter rules remaining)
      (cond
        [(empty? rules) (car (set->list remaining))]
        [else (iter (cdr rules) (set-remove remaining (caar rules)))]))
    (iter rules (get-all rules)))

  (define (get-first rules)
    (define (iter rules remaining)
      (cond
        [(empty? rules) (car (set->list remaining))]
        [else (iter (cdr rules) (set-remove remaining (cadr (car rules))))]))
    (iter rules (get-all rules)))

  (define (remove-rules-with rules val)
    (filter (lambda (rule) (not (index-of rule val))) rules))

  (define num-elements (length (set->list (get-all rules))))

  (define (iter rules order)
    (println order)
    (cond
      [(empty? rules) order]
      [else (iter (remove-rules-with rules (get-last rules)) (cons (get-last rules) order))]))

  (cons (get-first rules) (iter rules `()) ))

(define (sorted-pages sorted pages)
  (define (in-pages val)
    (cond
      [(index-of pages val) val]
      [else 0]))
  (filter positive? (map in-pages sorted)))

(define (pair-swapped pages index)
  (append (take pages index) (list (list-ref pages (++ index)) (list-ref pages index)) (drop pages (+ index 2))))

(define (sort-pages rules pages)
  (define (pass rules pages index)
    (cond
      [(= index (- (length pages) 1)) pages]
      [(breaks-any-rule (list (list-ref pages index) (list-ref pages (++ index))) rules) (pass rules (pair-swapped pages index) (++ index))]
      [else (pass rules pages (++ index))]))
  (define (iter rules pages pass-count)
    (cond
      [(pages-invalid rules pages) (iter rules (pass rules pages 0) (++ pass-count))]
      [else pages]))
  (iter rules pages 0))

(define (solve01 rules all-pages total)
  (cond
    [(empty? all-pages) total]
    [(pages-invalid rules (car all-pages)) (solve01 rules (cdr all-pages) total)]
    [else (solve01 rules (cdr all-pages) (+ total (mid-value (car all-pages))))]))

(define (solve02 rules  all-pages total)
  (cond
    [(empty? all-pages) total]
    [(pages-invalid rules (car all-pages)) (solve02 rules (cdr all-pages) (+ total (mid-value (sort-pages rules (car all-pages)))))]
    [else (solve02 rules (cdr all-pages) total)]))


(define rp (get-rules-and-pages "05.txt"))
(define rules (car rp))
(define all-pages (cadr rp))
(print (solve02 rules all-pages 0 ))
