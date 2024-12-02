(defun get-report (vals str start)
  (if (>= start (length str))
    (reverse vals)
    (get-report
      (cons (read-from-string str t nil :start start) vals)
      str
      (nth-value 1 (read-from-string str t nil :start start))
      )))


(defvar all-reports `())
(defun get-all-reports (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
        while line do 
        (setq all-reports (cons (get-report `() line 0) all-reports))
      )
      (close in)))
  (reverse all-reports )
)

(defun absdiff (v1 v2)
  (if (> v1 v2)
    (- v1 v2)
    (- v2 v1)))

(defun index-removed (vals index)
  (defun index-removed-rec (vals index past current)
    (if (equal index current)
      (concatenate `list (reverse past) (cdr vals))
      (index-removed-rec (cdr vals) index (cons (car vals) past) (+ current 1))))
  (index-removed-rec vals index `() 0)
  
  )

(defun sign-matches (v1 v2 sign)
  (equal (< v1 v2) sign))

(defun report-is-safe-rec (report asc index)
  (if (equal (cadr report) nil)
    (+ index 1)
    (if 
      (and 
        (>= (absdiff (car report) (cadr report)) 1)
        (<= (absdiff (car report) (cadr report)) 3)
        (sign-matches (car report) (cadr report) asc ))
      (report-is-safe-rec (cdr report) asc (+ index 1))
      (+ index 1)
      )))

(defun report-is-safe (report)
  (eq (report-is-safe-rec report (< (car report) (cadr report) ) 0)  (length report) ))

(defun safe-without-damping ()
  t
)

(defun check-all-dampings (report)
  (defun check-all-dampings-rec (report index)
    (if (eq index (length report))
      nil
      (if (report-is-safe (index-removed report index))
        t
        (check-all-dampings-rec report (+ index 1)))))
  (check-all-dampings-rec report 0))

(defun damped-report-is-safe (report)
  (if (report-is-safe report)
    (safe-without-damping)
    (check-all-dampings report)))
    

(defun get-safe-report-count (reports score)
  (if (equal (car reports) nil)
    score
  (if (report-is-safe (car reports))
    (get-safe-report-count (cdr reports) (+ score 1))
    (get-safe-report-count (cdr reports) score))))

(defun get-safe-damped-report-count (reports score)
  (if (equal (car reports) nil)
    score
  (if (damped-report-is-safe (car reports))
    (get-safe-damped-report-count (cdr reports) (+ score 1))
    (get-safe-damped-report-count (cdr reports) score))))

(print (get-safe-damped-report-count (get-all-reports "02.txt") 0))
