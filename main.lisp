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
     :accessor x-or-o)
   (turn-number
     :initarg :turn
     :initform nil
     :accessor turn)))

(defun make-piece (position type &optional turn)
  "Piece constructor"
  (make-instance 'piece :piece-position position :x-or-o type :turn turn))

(defun show-attributes (piece)
  "For debugging purposes: show the attributes for any given piece"
  (format t "Position: ~A~%Type: ~A~%Turn: ~A~%" (piece-position piece) (x-or-o piece) (turn piece)))

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

(defparameter p2 (make-piece (pos 2 3) 'x))

(defun check-slot (board slot)
  "Check which slots are available and taken. Output the last available y slot in given x slot or nil"
  (let ((available '()))
    (if (or (> slot 6) (< slot 0)) ; check if in range
        (format t "Not in range")
        (loop for x to 6
           do (if (string= (aref board x slot) "-")
                  ;(progn
                    (push x available)
                    ;(format t "x:~A y:~A Available~%" slot x))
                  ;(format t "x:~A y:~A Taken~%" slot x)
                )))
    (if available ; if everything taken, return nil
        (apply #'max available) ; get the last available y position
        nil)))

(drop-piece *board* 2)
(display-board *board*)

(defun drop-piece (board x-slot piece-type)
  (if (check-slot board x-slot)
      (progn
        (setf (piece-position p2) (pos x-slot (check-slot board x-slot)))
        (setf (x-or-o p2) piece-type)
        (update-board board p2))
      (progn
        (format t "no available moves on this slot")
        nil)))

(drop-piece *board* 1)

(defun play-turn (turn board)
  (format t "Choose a slot (1-7)~%~%1 2 3 4 5 6 7~%")
  (display-board board)
  (let ((slot (read))
        (piece-type 'x)
        (trunc-turn (cadr (multiple-value-list (truncate turn)))))
    (if (= trunc-turn 0.5)
        (setf piece-type 'o))
    (if (null (drop-piece board slot piece-type))
        (progn
          (format t "Try again.~%")
          (play-turn turn board)))
    (play-turn (+ turn 0.5) board)))

(play-turn *turn* *board*)
