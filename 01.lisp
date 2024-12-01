(defun insert-rec (smaller larger val)
  (if (equal (car larger) nil)
    (reverse (cons val smaller))
    (if (> (car larger) val)
      (concatenate `list (reverse (cons val smaller)) larger)
      (insert-rec (cons (car larger) smaller) (cdr larger) val))))

;; insert val into vals. Assumes vals is asc sorted list
(defun insert (vals val)
  (insert-rec `() vals val))

(defun get-second (str)
    (read-from-string str t nil :start (nth-value 1 (read-from-string str))))

(defun absdiff (v1 v2)
  (if (> v1 v2)
    (- v1 v2)
    (- v2 v1)))

(defun sumdist (total l1 l2)
  (if (equal (car l1) nil)
    total
    (sumdist (+ total (absdiff (car l1) (car l2))) (cdr l1) (cdr l2))))

(defun simscore (total l1 l2)
  (if (equal (car l1) nil)
    total
    (simscore (+ total (* (car l1) (count (car l1) l2))) (cdr l1) l2)))

(defvar list1 `())
(defvar list2 `())

(defun solve_p1 (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
        while line do 
        (setq list1 (insert list1 (read-from-string line)))
        (setq list2 (insert list2 (get-second line)))
          )
      (close in)))
  (print (sumdist 0 list1 list2))
  )

(defun solve_p2 (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
        while line do 
        (setq list1 (insert list1 (read-from-string line)))
        (setq list2 (insert list2 (get-second line)))
          )
      (close in)))
  (print (simscore 0 list1 list2))
  )

(solve_p2 "01.txt")

