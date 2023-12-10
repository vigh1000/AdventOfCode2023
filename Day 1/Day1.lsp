(defparameter *input-day-one-test* (list "1abc2" "pqr3stu8vwx" "a1b2c3d4e5f" "treb7uchet"))

(defun day1 ()
    "https://adventofcode.com/2023/day/1"
    (sum-calibration-value (get-input-from-file "./Day1_Input.txt"))
)

(defun day1-test ()
    (sum-calibration-value *input-day-one-test*)
)

(defun sum-calibration-value (input)
    (let (sum)
        (setq sum (mapcar #'get-digits-from-line input))
        (reduce #'+ sum :initial-value 0)
    )
)

(defun get-digits-from-line (line-as-string)
    (let (return-value return-value-1 return-value-2 string-as-char-list)        
        (setq string-as-char-list (coerce line-as-string 'list))
        (setq return-value-1 (digit-char-p (find-if #'digit-char-p string-as-char-list)))
        (setq return-value-2 (digit-char-p (find-if #'digit-char-p (reverse string-as-char-list))))
        ;(setq return-value (parse-integer (concatenate 'string (write-to-string return-value-1) (write-to-string return-value-2))))
        (setq return-value (+ (* 10 return-value-1) return-value-2))
    )
)

(defun get-input-from-file (filepath)
    (let ((in (open filepath)) result-list)
        (loop for line = (read-line in nil)
            while line do (push line result-list))
        (close in)
        result-list
    )
)

(print "Good luck!")