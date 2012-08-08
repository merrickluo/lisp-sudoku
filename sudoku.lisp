;; lym create to solve sudoku

(defparameter sudoku (make-array '(9 9) :initial-element 0))
(defparameter blocks (make-array '(9 9) :initial-element 0))
(defparameter colums (make-array '(9 9) :initial-element 0))
(defparameter memos (make-array '(9 9) :initial-element 0))

(defun convert-x (i j)
  "convert normal sudoku x to block x"
  (+ (* (floor (/ i 3)) 3) (floor (/ j 3))))

(defun convert-y (i j)
  "convert normal sudoku y to block y"
  (+ (* (mod i 3) 3) (mod j 3)))

(defun get-numbers()
  "get sudoku numbers from std input not recommend"
  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
	    (setf (aref sudoku i j) (read))
	    (setf (aref blocks (convert-x i j) (convert-y i j)) (aref sudoku i j))
	    (setf (aref colums j i) (aref sudoku i j)))))

(defun update-table (number i j)
  "must update all the tables when you change a number"
  (setf (aref sudoku i j) number)
  (setf (aref colums j i) number)
  (setf (aref blocks (convert-x i j) (convert-y i j)) number)
  (setf (aref memos i j) nil))

(defun solve-sudoku ()
  "main function to solve the sudoku"
  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
            (when (eq (aref sudoku i j) 0)
              (let ((row (make-array 9 :displaced-to sudoku
                                     :displaced-index-offset (* 9 i)))
                    (col (make-array 9 :displaced-to colums
                                     :displaced-index-offset (* 9 j)))
                    (blk (make-array 9 :displaced-to blocks
                                     :displaced-index-offset (* 9 (convert-x i j))))
                    (memo nil))
                (loop for k from 1 to 9 do
                     (when
                         (not (or (find k row)
                                  (find k col)
                                  (find k blk)))
                       (update-table k i j)
                       (if (or
                            (not (and (find 0 row)
                                      (find 0 col)
                                      (find 0 blk))))
                           nil
                           (progn
                             (update-table 0 i j)
                             (setf memo (cons k memo))))))
                (setf (aref memos i j) memo)))))
  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
            (let ((memo (aref memos i j)))
              (if (car memo)
                  (if (cdr memo)
                      nil
                      (update-table (car memo) i j)))))))
		       
(defun sudoku-solved ()
  "see if sudoku is solved"
  (let ((flag t))
    (loop for i from 0 to 8 do
	 (loop for j from 0 to 8 do
	      (if (eq (aref sudoku i j) 0)
		  (progn
		    (setf flag nil)
		    (return nil)))))
    flag))
  

(defun get-sudoku ()
  "try to solve sudoku until it is solved"
  (when (not (sudoku-solved))
      (solve-sudoku)
      (get-sudoku)))

(defun sudoku-cheat ()
  "enter function"
  ;;(get-numbers)
  (sudoku-from-file)
  (get-sudoku))

(defun sudoku-from-file (&optional (filename "~/lisp/lisp-sudoku/sudoku.data"))
  "read sudoku init data file"
  (with-open-file (filestream filename)
    (let ((i 0)
          (j 0))
      (loop for num =  (read filestream nil 'eof)
         until (eq num 'eof) do
           (when (not (eq num 'eof))
             (setf (aref sudoku i j) num)
             (setf (aref blocks (convert-x i j) (convert-y i j)) num )
             (setf (aref colums j i) num)
             (setf (aref memos i j) nil)
             (incf j)
             (when (> j 8)
                 (setf j 0)
                 (incf i)))))))
         
