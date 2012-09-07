;; Declare the global database variable
(defvar *db* nil)


;; Returns a CD-Record with the given parameters as a list
;; @param title the title of the CD
;; @param artist the CD artist
;; @param rating a user rating of the CD
;; @param ripped a boolean flag of whether this CD has been ripped
;; @return  a CD-Record
(defun make-cd (title artist rating ripped)
  (list :title title
        :artist artist
        :rating rating
        :ripped ripped
  )
)


;; Adds a previously created cd record to the database
;; @param cd the cd-record to insert to the database
(defun add-record(cd)
  (push cd *db*)
)


;; Loops over all the elements of *db* with the DOLIST macro
;; and prints the cd using a special format.
(defun dump-db ()
  (dolist (cd *db*)
    (format t "~{~a:~10t~a~%~}~%" cd)))


;; Auxiliary function to read the input from a prompt which
;; is presented to the user.
;; @param prompt a prompt to present to the user
;; @return the line the user input in response to the prompt
(defun prompt-read(prompt)
  (format *query-io* "~a: " prompt)
  (force-output *query-io*)
  (read-line *query-io*)
)

;; Makes a new CD record from data it gets by prompting
;; for each value in turn.
(defun prompt-for-cd ()
  (make-cd
   (prompt-read "Title")
   (prompt-read "Artist")
   (prompt-read "Rating")
   (prompt-read "Ripped [y/n]")
   )
)
   
