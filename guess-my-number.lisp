;; In this game, you pick a number from 1 to 100, and the computer has to guess it.
;;  1) Determine the upper and lower limit
;;  2) Guess a number in between these two numbers
;;  3) If the player says the number is smaller, lower the big limit
;;     Else, raise the small limit

;; A variable defined globally is called a top-level definition
;; It is convention to denote global vars with earmuffs

;; These are the upper and lower bounds for guessing.
(defparameter *small* 1) 
(defparameter *big* 100)

;; This function takes the upper and lower bounds for guessing,
;; and guesses the average number between them.
(defun guess-my-number () 
  (ash (+ *small* *big*) -1))
  ;; 'ash' shifts the second arguments bits according to the third argument

(defun smaller ()
  (setf *big* (1- (guess-my-number)))
  (guess-my-number))

(defun bigger ()
  (setf *small* (1+ (guess-my-number)))
  (guess-my-number))

;; This function takes care of the game loop
(defun start-over ()
  (defparameter *small* 1)
  (defparameter *bigger* 100)
  (guess-my-number))
