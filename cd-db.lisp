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


;; Save the database to a file given by the filename.
;; @param filename the name of the output database file 
(defun save-db (filename)
  (with-open-file (out filename
                       :direction :output
                       :if-exists :supersede)
    (with-standard-io-syntax
      (print *db* out)
     )
   )
)


;; Loads the database from the file given by the filename.
;; @param filename the name of the input database file
(defun load-db (filename)
  (with-open-file (in filename)
    (with-standard-io-syntax
      (setf *db* (read in))
    )
  )
)


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
   ;; either, get me the rating read from the prompt or assign it 0 if
   ;; none can be correctly parsed.
   (or (parse-integer (prompt-read "Rating"):junk-allowed t) 0) 
   (y-or-n-p "Ripped [y/n]") ;;yes or no prompt
   )
)


;; Add a bunch of CDs until the user gets tired of
;; inputing CDs.
(defun add-cds()
  (loop (add-record (prompt-for-cd))
     (if (not (y-or-n-p "Another? [y/n]: "))
     (return)
     )
  )
)
   

;; Query the database according to the anonymous function that is
;; passed in.  
(defun select (selector-fn)
  (remove-if-not selector-fn *db*)
)



(defun where (&key title artist rating (ripped nil ripped-p))
  #'(lambda(cd)
      (and
       (if title    (equal (getf cd :title)  title)  t)
       (if artist   (equal (getf cd :artist) artist) t)
       (if rating   (equal (getf cd :rating) rating) t)
       (if ripped-p (equal (getf cd :ripped) ripped) t)
      )
    )
)
