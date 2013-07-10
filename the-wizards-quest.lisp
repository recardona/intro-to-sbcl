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
	   (at-loc-p (obj)
	     ;; The cadr of the obj-locs list contains location information
	     (eq (cadr (assoc obj obj-locs)) loc)))
    ;; Removes from the parameter list objs elements which fail
    ;; the predicate test #'at-loc-p
    (remove-if-not #'at-loc-p objs)))




