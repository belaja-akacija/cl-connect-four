;;;; Connect four game implemented in common lisp

(defparameter *board* (make-array '(7 7) :initial-element '-))
(defparameter *turn* 0)

(defun update-turn (&optional reset)
    (if reset
        (setf *turn* 0)
        (setf *turn* (1+ *turn*))))

(defun pos (x y)
  "Position of the piece"
  `(:x ,x :y ,y))

(defclass piece ()
  ;;; assign the position based off the available y slot.
  ((position
     :initarg :piece-position
     :initform (pos 0 0)
     :accessor piece-position)
   (x-or-o
     :initarg :x-or-o
     :initform nil
     :accessor x-or-o)))

(defun make-piece (position type)
  "Piece constructor"
  (make-instance 'piece :piece-position position :x-or-o type))

(defun show-attributes (piece)
  "For debugging purposes: show the attributes for any given piece"
  (format t "Position: ~A~%Type: ~A~%" (piece-position piece) (x-or-o piece)))

(defun display-board (board)
  "Displays the board"
  (dotimes (x 7)
    (dotimes (y 7)
      (if (= y 6)
          (format t "~A~%" (aref board x y))
          (format t "~A " (aref board x y))))))

(defun update-board (board piece)
  "Update the board position, with the given piece coordinates"
  (setf (aref board (getf (piece-position piece) :y) (getf (piece-position piece) :x)) (x-or-o piece)))

(defun check-slot (board slot)
  "Check which slots are available and taken. Output the last available y slot in given x slot or nil"
  (let ((available '()))
    (if (or (> slot 6) (< slot 0)) ; check if in range
        (format t "Not in range")
        (loop for x to 6
           do (if (string= (aref board x slot) "-")
                    (push x available))))
    (if available ; if everything taken, return nil
        (apply #'max available) ; get the last available y position
        nil)))

(defun drop-piece (board x-slot piece-type)
  (let ((piece (make-piece (pos x-slot 0) piece-type))
        (y-slot (check-slot board x-slot)))
    (if y-slot
       (progn
         (setf (getf (piece-position piece) :y) y-slot) ; set y value of piece, based on what check-slot outputs
         (update-board board piece))
       (progn
         (format t "no available moves on this slot")
         nil))))

(defun main (turn board)
  (format t "Choose a slot (1-7)~%~%1 2 3 4 5 6 7~%")
  (display-board board)
  (let ((slot (read))
        (piece-type 'x)
        (trunc-turn (cadr (multiple-value-list (truncate turn)))))
    (if (= trunc-turn 0.5)
    (setf piece-type 'o))
    (cond
      ((check-win-p board)
       (format t "Win!"))
    ((null (drop-piece board (1- slot) piece-type))
          (format t "~%Try again.~%")
          (main turn board)))
    ;; recursively run loop, incrementing turn by 0.5
    (main (+ turn 0.5) board)))

(defun check-win-p (board)
  (format nil "will fill this out soon")
  (cond ((horizontal-check board 0)
         t)))
        ;(t nil)))

(defun horizontal-check (board x &optional win)
  (let ((counter 0)
        )
    ;; if x = 7, return nil (nothing was found anyway)
    (if (> x 6)
        nil
        (loop for y from 0 to 6
              ;; stop loop if counter is equal to 4
           until (= counter 4)
           do (progn
                ;(format t "Checking slot: ~A,~A~%" x y)
                ;; if slot is equal to piece and the slot ahead of it is not another piece type, set counter +1
                ;; that is, count only if in line
                (if (and (eq (aref board y x) 'x) (not (eq (aref board (if (= y 6) y (1+ y)) x) 'o)))
                    (progn
                      ;(format t "Found an X.~%")
                      (setf counter (1+ counter)))))
           finally (cond ((>= counter 4)
                          (format t "Win!") ; TODO Figure out why it doesn't return T
                          (setf win t))
                        ;; while x < size of board, recurse function with x + 1
                        ((< x 7)
                            (horizontal-check board (1+ x) win)))))
    (print win)
    win)) ; TODO even when explicitly returning T in the form of win

;(horizontal-check *board* 0)

;(aref *board* 4 2) ; *board* y,x
;(display-board *board*)
