;; lym create to solve sudoku

(defun solve-sudoku ()
  "main function to solve the sudoku"
  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
            (when (eq (laref sudoku i j) 0)
              (let* ((row (nth i sudoku))
		     (col (nth j colums))
		     (blk (nth (convert-x i j) blocks))
		     (memo nil)
                     (other (merge-lists row col blk #'< #'eq))
                     (row-up (nth (s- i) sudoku))
		     (row-down (nth (s+ i) sudoku))
		     (col-left (nth (s- j) colums))
		     (col-right (nth (s+ j) colums))
                     (other-list (intersection (intersection row-up row-down :test #'eq)
                                               (intersection col-left col-right :test #'eq)
                                               :test #'eq)))
                (loop for k from 1 to 9
                   while (eq (laref sudoku i j) 0) do
		     (when
                          (not (or (find k row)
                                   (find k col)
                                   (find k blk)))
                       (update-table k i j)
                       (if (or
                            (find k other-list)
                            (not (and (find 0 row)
                                      (find 0 col)
                                      (find 0 blk)))
			    (eq (length other) 9))
;;                           (format t "i:~a j:~a k:~a~%" i j k)
                           (progn
                             (update-table 0 i j)
                             (setf memo (cons k memo))))))
                (setf memo (laref memos i j))))))

  (loop for i from 0 to 8 do
       (loop for j from 0 to 8 do
            (let ((memo (nth j (nth i memos))))
              (if (car memo)
                  (if (cdr memos)
                      nil
                      (update-table (car memo) i j)))))))
		       
(defun sudoku-solved ()
  "see if sudoku is solved"
  (let ((flag t))
    (loop for i from 0 to 8 do
         (if (find 0 (nth i sudoku))
             (setf flag nil)))
    flag))

(defun get-sudoku ()
  "try to solve sudoku until it is solved"
  (when (not (sudoku-solved))
      (solve-sudoku)
      (get-sudoku)))

(defun sudoku-cheat ()
  "enter function"
  (sudoku-from-file)
  (get-sudoku)
  sudoku)

(defun init-data ()
  " init helper data structure"
  (defparameter sudoku (make-list 9 :initial-element nil))
  (defparameter blocks (make-list 9 :initial-element nil))
  (defparameter colums (make-list 9 :initial-element nil))
  (defparameter memos (mapcar (lambda (elt)
                        (setf elt (make-list 9 :initial-element nil)))
                      (make-list 9 :initial-element nil))))

(defun sudoku-from-file (&optional (filename "~/workspace/lisp/sudoku/sudoku.data"))
  "read sudoku init data file"
  (init-data)
  (with-open-file (filestream filename)
    (loop for i from 0 to 8 do
         (loop for j from 0 to 8
            for num =  (read filestream nil 'eof)
            until (eq num 'eof) do
              (push num (nth i sudoku))
              (push num (nth j colums))
              (push num (nth (convert-x i j) blocks)))))
  (setf sudoku (mapcar #'reverse sudoku))
  (setf colums (mapcar #'reverse colums))
  (setf blocks (mapcar #'reverse blocks)))
         
(defun merge-lists (list-a list-b list-c sort-fn test-fn)
  "merge two list together"
  (sort (remove-duplicates (append list-a list-b list-c ) :test test-fn) sort-fn))

(defun s- (x)
  (if (eq (mod x 3) 0)
      (+ x 2)
      (- x 1)))

(defun s+ (x)
  (if (eq (mod x 3) 2)
      (- x 2)
      (+ x 1)))
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
	    (push (nth i sudoku) (read))
	    (push (nth (convert-x i j) blocks) (car (nth i sudoku)))
	    (push (nth j colums) (car (nth i sudoku))))))

(defun update-table (number i j)
  "must update all the tables when you change a number"
  (setf (laref sudoku i j) number)
  (setf (laref colums j i) number)
  (setf (laref blocks (convert-x i j) (convert-y i j)) number)
  (setf (laref memos i j) nil))

(defmacro laref (list x y)
  `(nth ,y (nth ,x ,list)))
