#lang racket

(define (get-grid filename)
  (define in (open-input-file filename))
  (define (iter grid)
    (define line (read-line in `any))
    (cond
      [(eof-object? line) grid]
      [else (iter (cons (string->list line) grid))]))
  (iter `()))

(define (get-char grid pos)
  (cond
    [(< (car pos) 0) #\Q]
    [(< (cadr pos) 0) #\Q]
    [(>= (car pos) (length grid)) #\Q]
    [(>= (cadr pos) (length (car grid))) #\Q]
    [else (list-ref (list-ref grid (car pos)) (cadr pos))]))

(define (add-pos pos diff)
  (list (+ (car pos) (car diff)) (+ (cadr pos) (cadr diff))))

(define (++ val)
  (+ val 1))

(define xmas `(#\X #\M #\A #\S))

(define (check-xmas grid pos diff index)
  (cond
    [(= index 4) 1]
    [(equal? (get-char grid pos) (list-ref xmas index)) (check-xmas grid (add-pos pos diff) diff (++ index))]
    [else 0]))

(define directions `((0 1) (1 1) (1 0) (1 -1) (0 -1) (-1 -1) (-1 0) (-1 1)))
(define (check-all grid pos dir-index total)
  (cond
    [(= dir-index (length directions)) total]
    [else 
      (check-all 
        grid 
        pos 
        (++ dir-index) 
        (+ total (check-xmas grid pos (list-ref directions dir-index) 0)))]))

(define (is-mas grid pos diffs)
  (define (is-ms c1 c2)
    (cond
      [(and (equal? c1 #\M) (equal? c2 #\S)) true]
      [(and (equal? c1 #\S) (equal? c2 #\M)) true]
      [else false]))
  (is-ms (get-char grid (add-pos pos (car diffs))) (get-char grid (add-pos pos (cadr diffs)))))

(define cross-diffs `( ((1 1) (-1 -1)) ((1 -1) (-1 1)) ))
(define (check-cross grid pos)
  (cond
    [(and (equal? (get-char grid pos) #\A) (is-mas grid pos (car cross-diffs)) (is-mas grid pos (cadr cross-diffs))) 1]
    [else 0]))

(define (next-pos grid pos)
  (cond
    [(< (car pos) (- (length grid) 1)) (list (+ (car pos) 1) (cadr pos))]
    [(< (cadr pos) (- (length (car grid)) 1)) (list 0 (+ (cadr pos) 1))]
    [else null]))
       
(define (find-matches grid)
  (define (iter grid pos total)
    (cond
      [(null? pos) total]
      [else (iter grid (next-pos grid pos) (+ total (check-all grid pos 0 0)))]))
  (iter grid `(0 0) 0))

(define (find-crosses grid)
  (define (iter grid pos total)
    (cond
      [(null? pos) total]
      [else (iter grid (next-pos grid pos) (+ total (check-cross grid pos))) ]))
  (iter grid `(0 0) 0))

(print (find-crosses (get-grid "04.txt")) )
