(load "~/quicklisp/setup.lisp")
(ql:quickload "cl-ppcre")

;; if val is nil return length of string
(defun val-or-len (str val)
  (if (equal val nil)
    (length str)
    val))

(defun first-do (str)
  (val-or-len str (car (ppcre:all-matches "do\\(\\)" str))))

(defun first-dont (str)
  (val-or-len str (car (ppcre:all-matches "don\\'t\\(\\)" str))))

(defun get-enabled (str)
  (defun take-enabled-rec (enabled rem-str currently-enabled)
    (if (equal (length rem-str) 0)
      enabled
      (if currently-enabled
        (take-enabled-rec 
          (concatenate `string enabled (subseq rem-str 0 (first-dont rem-str))) 
          (subseq rem-str (first-dont rem-str))
          nil)
        (take-enabled-rec
          enabled
          (subseq rem-str (first-do rem-str))
          t))))
  (take-enabled-rec "" str t))

(defun get-uncorrupted (str)
  (ppcre:all-matches-as-strings "mul\\(\\d\\d?\\d?,\\d\\d?\\d?\\)" str))

(defun run-calculation (calc)
  (*
    (read-from-string calc t nil :start 4)
    (read-from-string calc t nil :start (+ 1 (nth-value 1 (read-from-string calc t nil :start 4))))))

(defun sum-all-calculations (calcs total)
  (if (equal (car calcs) nil)
    total
    (sum-all-calculations (cdr calcs) (+ total (run-calculation (car calcs))))))

(defvar total 0)
(defun solve1 (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
        while line do 
        (setq total (+ total (sum-all-calculations (get-uncorrupted line) 0)))
      )
      (close in)))
  (print total)
)

(defvar full-mem "")
(defun solve2 (filename)
  (let ((in (open filename :if-does-not-exist nil)))
    (when in
      (loop for line = (read-line in nil)
        while line do 
        (setq full-mem (concatenate `string full-mem line))
      )
      (close in)))
  (print (sum-all-calculations (get-uncorrupted (get-enabled full-mem)) 0))
)

(solve2 "03.txt")
