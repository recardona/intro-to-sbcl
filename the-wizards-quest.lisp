;;
;; the-wizards-quest.lisp
;; @author: recardona 
;;
;; In this game, you are a wizard's apprentice.  You'll explore the wizard's
;; house.  You can visit three different locations: a living room, an attic,
;; and a garden. You can look around, walk to different locations, pick up
;; objects, and perform actions on those objects (pending).  
;;

;; Association (a-)list for game location nodes.
;; Nodes contain a simple description of the location itself.
(defparameter *nodes* '( (living-room (You are in the living room.
				       A wizard is snoring loudly on the couch.))
			 (garden (You are in a beautiful garden.
				  There is a well in front of you.))
			 (attic (You are in the attic.
				 There is a giant welding torch in the corner.))))

;; Association (a-)list for game location edges.
;; Edges contain information of relative position to the key of the a-list,
;; as well as the object that connects the key of the a-list to each of the
;; datums it's paired to. 
;; 
;; e.g. there is an edge between the living room to the garden,
;;  which places the garden to the west of the living room, 
;;  accessible through the door.
(defparameter *edges* '((living-room (garden west     door)
			             (attic  upstairs ladder))
			(garden (living-room east door))
			(attic  (living-room downstairs ladder))))


;; List of objects in the game world.
(defparameter *objects* '(whiskey bucket frog chain))

;; Alist of object locations.
(defparameter *object-locations* '((whiskey living-room)
				   (bucket living-room)
				   (chain garden)
				   (frog garden)))

;; Player's starting location.
(defparameter *location* 'living-room)



;; Using the assoc function, which searches an a-list in order,
;; and returns the key-datum pairs, find "location" within "nodes," 
;; and get the head element of its tail.  This should return the 
;; description of the specified location in the a-list, as a single list.
(defun describe-location (location nodes)
  (cadr (assoc location nodes)))

;; Using quasiquoting [`,], build a list textual description of a
;; given edge.
(defun describe-path (edge)
  `(There is a ,(caddr edge) going ,(cadr edge) from here.))

;; Find the relevant edges, convert the edges to descriptions, and
;; join the descriptions. Apply takes each list item, and treats it
;; as a parameter to the function it seeks to apply.
(defun describe-paths (location edges)
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

;; Find the objects (objs) at the parameter (loc) location.
(defun objects-at (loc objs obj-locs)
  (labels (;; Predicate function the answers:
	   ;; Is the object obj at location loc?
	   ;; From Wikipedia: (In logic) a predicate is (informally) a 
	   ;; statement that may be true or false depending on the values
	   ;; of its variables. It’s a Common Lisp convention to append a
	   ;; p to the end of predicate function names
	   (at-loc-p (obj)
	     ;; The cadr of the obj-locs list contains location information
	     (eq (cadr (assoc obj obj-locs)) loc)))
    ;; Removes from the parameter list objs elements which fail
    ;; the predicate test #'at-loc-p
    (remove-if-not #'at-loc-p objs)))

;; Describe the objects that can be found at the location (loc)
(defun describe-objects (loc objs obj-locs)
  (labels (
	   (describe-obj (obj)
	     `(you see a ,obj on the floor.)))
    (apply #'append (mapcar #'describe-obj (objects-at loc objs obj-locs)))))


;; ========================== Game Commands ==========================
;; Remember that because these functions depend on a global variable, they are
;; not in the "functional style," which denotes functions that are completely
;; determined by the inputs to that function, and which always give the same
;; output with respect to the same inputs. Functional syle functions are
;; functions in the mathematical sense.


;; The game's 'look' command.
;; Players use this function to describe the world around them.
(defun look ()
  (append (describe-location *location* *nodes*)
	  (describe-paths *location* *edges*)
	  (describe-objects *location* *objects* *object-locations*)))

;; The game's 'walk' command.
;; Players use this function to move around in the world.
;; Players can only walk to locations which are adjacent to them,
;; which are accessible toward specific directions.
(defun walk (direction)
  (let (
	;; Try to define the local variable 'next', by searching for
	;; a match for the parameter 'direction' in the directions 
	;; within the *edges* variable.
	(next (find direction 
		    (cdr (assoc *location* *edges*)) ;; this cdr yields the edge datums available from the current location
		     :key #'cadr)) ;; the cadr of the above list yields the direction through which the edge is established

	;; In essence, you're trying to see if you can find the direction you intend to walk through in the edges available
	;; from your current location.  If the above lookup fails, 'next' will be nil.  If it succeeds, 'next' will be the
	;; complete edge datum of the *edges* a-list (found by cdr, above).
	)

    (if next
	(progn
	  (setf *location* (car next)) ;; the car of next is the new location you're walking toward
	  (look)) ;; auto-look at the next location once you've walked to it

	'(You cannot go that way.) ;; if next is nil, just return the message saying you can't walk in the chosen direction
    )))
