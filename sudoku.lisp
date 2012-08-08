;; lym created

(defparameter sudoku (make-array '(9 9) :initial-element 0))
(defparameter blocks (make-array '(9 9) :initial-element 0))
(defparameter colums (make-array '(9 9) :initial-element 0))

(defun convert-x (i j)
  (+ (* (floor (/ i 3)) 3) (floor (/ j 3))))

(defun convert-y (i j)
  (+ (* (mod i 3) 3) (mod j 3)))

(defun get-numbers()
  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
	    (setf (aref sudoku i j) (read))
	    (setf (aref blocks (convert-x i j) (convert-y i j)) (aref sudoku i j))
	    (setf (aref colums j i) (aref sudoku i j)))))

(defun update-table (number i j)
  (setf (aref sudoku i j) number)
  (setf (aref colums j i) number)
  (setf (aref blocks (convert-x i j) (convert-y i j)) number))

(defun solve-sudoku ()
  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
            (when (eq (aref sudoku i j) 0)
              (let ((row (make-array 9 :displaced-to sudoku
                                     :displaced-index-offset (* 9 i)))
                    (col (make-array 9 :displaced-to colums
                                     :displaced-index-offset (* 9 j)))
                    (blk (make-array 9 :displaced-to blocks
                                     :displaced-index-offset (* 9 (convert-x i j)))))
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
                           (return)
                           (update-table 0 i j)))))))))

		       
(defun sudoku-solved ()
  (let ((flag t))
    (loop for i from 0 to 8 do
	 (loop for j from 0 to 8 do
	      (if (eq (aref sudoku i j) 0)
		  (progn
		    (setf flag nil)
		    (return nil)))))
    flag))
  

(defun get-sudoku ()
  (when (not (sudoku-solved))
      (solve-sudoku)
      (get-sudoku)))

(defun sudoku-cheat ()
  ;;(get-numbers)
  (sudoku-from-file)
  (get-sudoku))

(defun sudoku-from-file (&optional (filename "~/lisp/sudoku.data"))
  (with-open-file (filestream filename)
    (let ((i 0)
          (j 0))
      (loop for num =  (read filestream nil 'eof)
         until (eq num 'eof) do
           (when (not (eq num 'eof))
             (setf (aref sudoku i j) num)
             (setf (aref blocks (convert-x i j) (convert-y i j)) num )
             (setf (aref colums j i) num)
             (incf j)
             (when (> j 8)
                 (setf j 0)
                 (incf i)))))))
         
