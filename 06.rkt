#lang racket
 (require racket/treelist)

(define (atomize char)
  (cond
    [(equal? char #\.) `unvisited]
    [(equal? char #\X) `visited]
    [(equal? char #\#) `barrier]
    [(equal? char #\^) `up]
    [(equal? char #\>) `right]
    [(equal? char #\v) `down]
    [(equal? char #\<) `left]
    [else `whoops]))

(define (parse-map port)
  (list->treelist
    (for/list ([line (in-lines port `any)])
              (list->treelist (map atomize (string->list line))))
  ))

(define (get-atom patrol-map pos)
  (define row (first pos))
  (define col (second pos))
  (cond
    [(< row 0) `oob]
    [(< col 0) `oob]
    [(>= row (treelist-length patrol-map)) `oob]
    [(>= col (treelist-length (treelist-first patrol-map))) `oob]
    [else (treelist-ref (treelist-ref patrol-map row) col)]))

(define (is-player char)
  (cond
    [(equal? char `up) true]
    [(equal? char `right) true ]
    [(equal? char `down) true]
    [(equal? char `left) true]
    [else false]))

(define (off-map patrol-map guard-position)
  (define row (car guard-position))
  (define col (cadr guard-position))
  (cond
    [(< row 0) true]
    [(< col 0) true]
    [(>= row (treelist-length patrol-map)) true]
    [(>= col (treelist-length (treelist-first patrol-map))) true]
    [else false]))

(define (find-guard patrol-map)
  (for*/first ([row (build-list (treelist-length patrol-map) values)]
               [col (build-list (treelist-length (treelist-first patrol-map)) values )]
               #:when (is-player (get-atom patrol-map (list row col)))
              )
    (list (get-atom patrol-map (list row col)) (list row col))))

(define (visited-count patrol-map)
  (for*/sum ([row (build-list (treelist-length patrol-map) values)]
             [col (build-list (treelist-length (treelist-first patrol-map)) values )]
            )
      (if (equal? (get-atom patrol-map (list row col)) `visited) 1 0)))

(define (add-pos pos diff)
  (list (+ (car pos) (car diff)) (+ (cadr pos) (cadr diff))))

(define (dir-to-diff dir)
  (cond
    [(equal? dir `left) `(0 -1)]
    [(equal? dir `right) `(0 1)]
    [(equal? dir `down) `(1 0)]
    [(equal? dir `up) `(-1 0)]
    [else `(1000 1000)]))

(define (rotate dir)
  (cond
    [(equal? dir `up) `right]
    [(equal? dir `down) `left]
    [(equal? dir `left) `up]
    [(equal? dir `right) `down]
    [else `up]))

(define (update-map patrol-map updates)
  (define (update-pos patrol-map update)
    (define sym (car update))
    (define pos (cadr update))
    (define row (car pos))
    (define col (cadr pos))
    (cond
      [(equal? (get-atom patrol-map pos) `oob) patrol-map]
      [else (treelist-set patrol-map row (treelist-set (treelist-ref patrol-map row) col sym))]))
  (for/fold ([p-map patrol-map] #:result p-map)
            ([update updates])
            (update-pos p-map update)))

(define (can-visit atom)
  (cond
    [(equal? atom `unvisited) true]
    [(equal? atom `visited) true]
    [(equal? atom `oob) true]
    [else false]))

(define (run-sim in-patrol-map)
  (define guard (find-guard in-patrol-map))
  (define guard-direction (first guard))
  (define guard-position (second guard))
  (define patrol-map (update-map in-patrol-map (list (list `unvisited guard-position))))
  (for/fold ([position guard-position]
             [direction guard-direction]
             [visited (list->set `())]
             [loop false]
             #:result (list (set-count visited) loop)
            )
            ([i (build-list 100000 values)]
             #:break (or loop (off-map patrol-map position)))
            (cond
              [(can-visit (get-atom patrol-map (add-pos position (dir-to-diff direction))))
               (values (add-pos position (dir-to-diff direction)) 
                       direction 
                       (set-add visited (cons position direction))
                       (set-member? visited (cons position direction))
              )]
              [else
               (values position 
                       (rotate direction) 
                       (set-add visited (cons position direction))
                       (set-member? visited (cons position direction))
                       )]
            )
  )
)

(define (loop-count patrol-map)
  (if (cadr (run-sim patrol-map)) 1 0))

(define (solve01 patrol-map)
  (run-sim patrol-map))

(define (solve02 patrol-map)
  (define rows (treelist-length patrol-map))
  (define cols (treelist-length (treelist-first patrol-map)))
  ;; technically we need not loop through every position. we could just get
  ;; all the visited in the original run and then just check those.
  (for*/sum ([row (build-list rows values)]
             [col (build-list cols values)]
             #:unless (or (equal? `barrier (get-atom patrol-map (list row col)))
                          (is-player (get-atom patrol-map (list row col)))))
    (cond
      [else 
        (define new-map (update-map patrol-map (list (list `barrier (list row col)))))
        (define is-loop (cadr (run-sim new-map)))
        ;; (if is-loop (println (list row col`loop)) (println (list row col)))
        (if is-loop 1 0)
      ] )))

(define file (open-input-file "data/06_sample.txt"))
(define patrol-map (parse-map file))
(print (solve02 patrol-map))


