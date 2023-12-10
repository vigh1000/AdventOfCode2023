(defparameter *input-day-two-test* (list "Game 1; 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green" 
                                         "Game 2; 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue" 
                                         "Game 3; 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red" 
                                         "Game 4; 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red"
                                         "Game 5; 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green"))

(defun day2 ()
    "https://adventofcode.com/2023/day/2"
    (day2-work (read-file-to-list "Day2/Day2_Input.txt"))
)

(defun day2-test ()
    (day2-work *input-day-two-test*)
)

(defun day2-work (input)
    (let (game-list possible-game-list)
        (setq game-list (read-list-to-game-list input))
        (dolist (game game-list)
            (when (possible-game-p game)
                (push (parse-integer (subseq (car game) 2)) possible-game-list)
            )
        )
        (pprint possible-game-list)
        (reduce #'+ possible-game-list)
    )
)

(defun read-file-to-list (filepath)
    (let ((in (open filepath)) result-list)
        (loop for line = (read-line in nil)
            while line do (push line result-list))
        (close in)
        (nreverse result-list)
    )
)

(defun read-list-to-game-list (input-list)
    "Converts a list to a list-list where the games are a list of revelations in each game"
    (let (result-list game-list old-position new-position)
      (dolist (element input-list) 
        (setq old-position 0)
        (setq new-position 0)
        (setq game-list ())
        (dotimes (i (count #\; element))
           (setq new-position (position #\; element :start (1+ old-position)))
           (push (subseq element (+ 2 old-position) new-position) game-list)
           (when new-position
             (setq old-position new-position)
            )
        )
        (push (subseq element (+ 2 new-position)) game-list)
        (push (nreverse game-list) result-list)
      )
      (nreverse result-list)
    )
)

(defun possible-game-p (game)
    "Looks if game is possible with only 12 red cubes, 13 green cubes, and 14 blue cubes"
    (let ((result T))
        (dolist (revelation game)
            ;(pprint (get-number-of-cubes revelation "red"))       
            (when (> (get-number-of-cubes revelation "red") 12) (setq result NIL))         
            (when (> (get-number-of-cubes revelation "green") 13) (setq result NIL))         
            (when (> (get-number-of-cubes revelation "blue") 14) (setq result NIL))        
        )        
        result
    )
)

(defun get-number-of-cubes (revelation color)
    ; (print revelation)
    ; (print color)
    (let ((result 0))
        (when (search color revelation)
            (cond 
            ((plusp (- (search color revelation) 3))              
                ; (print (subseq revelation (- (search color revelation) 3) (- (search color revelation) 1)))
                (setq result (parse-integer (subseq revelation (- (search color revelation) 3) (- (search color revelation) 1)))))
            ((zerop (- (search color revelation) 2))              
                ; (print (subseq revelation 0 1))
                (setq result (parse-integer (subseq revelation 0 1))))
            ((zerop (- (search color revelation) 3))              
                ; (print (subseq revelation 0 2))
                (setq result (parse-integer (subseq revelation 0 2))))
            )
        )
        ; (print result)
        result
    )     
)

(defun get-game-number (game)
    (print game)
      
)

(print "Good luck!")