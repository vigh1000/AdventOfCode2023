(defparameter *input-day-one-test* (list "two1nine" "eightwothree" "abcone2threexyz" "xtwone3four" "4nineeightseven2" "zoneight234" "7pqrstsixteen"))

(defun day1 ()
    "https://adventofcode.com/2023/day/1"
    (let (input-transformed-to-numbers)
        (setq input-transformed-to-numbers (transform-input-to-numbers (get-input-from-file "./Day1_Input.txt")))
        (sum-calibration-value input-transformed-to-numbers)
    )
)

(defun day1-test ()
    (let (input-transformed-to-numbers)
        (setq input-transformed-to-numbers (transform-input-to-numbers *input-day-one-test*))
        (pprint "transformed list:")
        (pprint input-transformed-to-numbers)
        (sum-calibration-value input-transformed-to-numbers)
    )
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

;; funktioniert so nicht, wegen eightwothree => hier wird die zwei ersetzt und dann wird die 8 nicht mehr erkannt
(defun transform-input-to-numbers (input-list)
    (let ((return-sequence '()))
      (dolist (element input-list)
        (when (find-one-as-word element)  ;find-one-as-word müsste erweitert werden 
           (replace element "1" :start1 (+ (find-one-as-word element) 1) :end1 (+ (find-one-as-word element) 3))) ; nicht schön, weil der Rest von 'one' zurück bleibt
        (when (search "two" element)
           (replace element "2" :start1 (+ (search "two" element) 1) :end1 (+ (search "two" element) 3)))
        (when (search "three" element)
           (replace element "3" :start1 (+ (search "three" element) 1) :end1 (+ (search "three" element) 5)))
        (when (search "four" element)
           (replace element "4" :start1 (+ (search "four" element) 1) :end1 (+ (search "four" element) 4)))
        (when (search "five" element)
           (replace element "5" :start1 (+ (search "five" element) 1) :end1 (+ (search "five" element) 4)))
        (when (search "six" element)
           (replace element "6" :start1 (+ (search "six" element) 1) :end1 (+ (search "six" element) 3)))
        (when (search "seven" element)
           (replace element "7" :start1 (+ (search "seven" element) 1) :end1 (+ (search "seven" element) 5)))
        (when (search "eight" element)
           (replace element "8" :start1 (+ (search "eight" element) 1) :end1 (+ (search "eight" element) 5)))
        (when (search "nine" element)
           (replace element "9" :start1 (+ (search "nine" element) 1) :end1 (+ (search "nine" element) 4)))
        (push element return-sequence)
        )
      (nreverse return-sequence)
    )
)

(defun replace-number-if-found (element number-as-text number-as-digit) ;bringt so leider nichts, weil Rückgabewert nil, wenn nicht gefunden
    (when (search number-as-text element)
        (replace element number-as-digit :start1 (search number-as-text element) :end1 (+ (search number-as-text element) 3))) ; nicht schön, weil der Rest von 'one' zurück bleibt, also '1on'
)

(defun find-one-as-word (input)
    (search "one" input)
)

(print "Good luck!")

