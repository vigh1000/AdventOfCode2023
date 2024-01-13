(defparameter *input-day-three-test* (list 
                                           "............" 
                                           ".467..114..." 
                                           "....*......."
                                           "...35..633.."
                                           ".......#...."
                                           ".617*......."
                                           "......+.58.."
                                           "...592......"
                                           ".......755.."
                                           "....$.*....."
                                           "..664.598..."                                          
                                           "............"                                          
                                           ))

(defun day3 ()
    "https://adventofcode.com/2023/day/3"
    (day3-work (read-file-to-list "Day3/Day3_Input.txt"))
    (day2-work)
)

(defun day3-test ()
    (day3-work *input-day-three-test*)
)

(defun day3-work (input)
    (let (schematic-list)
        (setq schematic-list (read-list-to-game-list input))
        (setq schematic-list )
        (dolist (schematic schematic-list)
            (loop for char across schematic
                      position from 0
                      when (digit-char-p char)
                      do )
        )
        (pprint schematic-list)
        (reduce #'+ schematic-list)
    )
)

(defun get-schematic-numbers (schematic schematic-list)

)

(defun read-file-to-list (filepath)
    (let ((in (open filepath)) result-list)
        (loop for line = (read-line in nil)
            while line do (push line result-list))
        (close in)                
        (nreverse result-list)
    )
)

(print "Good luck!")