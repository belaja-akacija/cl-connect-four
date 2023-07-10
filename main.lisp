;;;; Connect four game implemented in common lisp

;;; Connect four rules:
;;; 2 players take turns stacking x's and o's on top of other x's and o's
;;; first to have 4 pieces in a row, diagonally, or on the x or y axis, that player wins
;;; cannot place piece unless there is a piece directly below it
;;; cannot move pieces or place in the same cell
;;;
;;; have to learn how to store the position of a given x or o
;;;    - grid[7][7]
;;;    # # # # # # # 0
;;;    # # # # # # # 1
;;;    # # # # # # # 2
;;;    # # # o x # # 3
;;;    # # o x o # # 4
;;;    # o x x o # # 5
;;;    # o x x x o # 6
;;;    0 1 2 3 4 5 6
;;;
;;; write algorithm that defines the possible legal moves in a given position
;;; track score
;;; store turn numbers
;;; print grid to screen
;;; define a generic data type for a piece that stores the
;;;  - x,y position
;;;  - turn number
;;;  - wether it's an x or an o

(defparameter *board* (make-array '(7 7) :initial-element '-))

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

(defun make-piece (position type turn)
  "Piece constructor"
  (make-instance 'piece :piece-position position :x-or-o type :turn turn))

(defun show-attributes (piece)
  (format t "Position: ~A~%Type: ~A~%Turn: ~A~%" (piece-position piece) (x-or-o piece) (turn piece)))

(defun display-board (board)
  (dotimes (x 7)
    (dotimes (y 7)
      (if (= y 6)
          (format t "~A~%" (aref board x y))
          (format t "~A " (aref board x y))))))
(setf (piece-position p1) (pos 1 1))

(defun update-board (board piece)
  (setf (aref board (getf (piece-position piece) :y) (getf (piece-position piece) :x)) (x-or-o piece)))

;;; The pieces must fall. Consider choosing only the top slot, then finding out what is the last available space it can go.

(defparameter p2 (make-piece (pos 2 3) 'x))

(defun check-slot (board slot)
  (let ((available '()))
    (loop for x to 6
         do (if (string= (aref board slot x) "-")
                (progn
                  (push x available)
                  (format t "x:~A y:~A Available~%" slot x))
                (format t "x:~A y:~A Taken~%" slot x)))
    (apply #'max available))) ; get the last available y position

(setf (piece-position p2) (pos 0 6))
(setf (x-or-o p2) 'x)
(check-slot *board* 1)
(update-board *board* p2)
(display-board *board*)
