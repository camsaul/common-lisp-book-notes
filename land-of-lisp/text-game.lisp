(defpackage :text-game
  (:use :cl))

;; defparameter - like Clojure def.
;; defvar - like Clojure defonce.
(defparameter *nodes*
  '((living-room (you are in the living room. a wizard is snoring loudly on the couch.))
    (garden (you are in a beautiful garden. there is a well in front of you.))
    (attic (you are in the attic. there is a giant welding torch in the corner.))))

(defun describe-location (location &optional (nodes *nodes*))
  (cadr (assoc location nodes)))

(defparameter *edges*
  '((living-room
     (garden west door)
     (attic upstairs ladder))
    (garden
     (living-room east door))
    (attic
     (living-room downstairs ladder))))

(defun describe-path (edge)
  `(there is a ,(caddr edge) going ,(cadr edge) from here.))

(defun describe-paths (location &optional (edges *edges*))
  (apply #'append (mapcar #'describe-path (cdr (assoc location edges)))))

(defparameter *objects* '(whiskey bucket chain frog))

(defparameter *object-locations*
  '((whiskey living-room)
    (bucket living-room)
    (chain garden)
    (frog garden)))

(defun objects-at (location &optional (objects *objects*) (object-locations *object-locations*))
  ;; labels -- define local functions. Like a let* version of flet
  ;;
  ;; Comparison rules:
  ;; 1.  Use eq to compare symbols. Sort of like Clojure identical?. Can also be used to compare two conses for being the same objects.
  ;;      eq is the simplest of all Lisp comparison functions, thus very fast.
  ;; 2.  Use equal for everything else. Sort of like Clojure = -- checks if things "look the same"
  ;; 3.  eql is like eq but can be used to compare numbers and characters as well
  ;; 4.  equalp is like equal but is case-insensitive for string comparisons and type-insensitive for numeric comparisions, i.e. 0 and 0.0 are equalp
  ;; 5.  = compares numbers, string-equal compares strings, and char-equal compares characters
  ;;
  ;; (assoc key list) returns matching (key value) pair from an association list.
  (labels ((at-location-p (object)
             (eq (cadr (assoc object object-locations)) location)))
    (remove-if-not #'at-location-p objects)))

(defun describe-objects (location &optional (objects *objects*) (object-locations *object-locations*))
  (labels ((describe-obj (object)
             `(you see a ,object on the floor.)))
    ;; mapcar = equivalent of Clojure or Scheme map.
    (apply #'append (mapcar #'describe-obj (objects-at location objects object-locations)))))

(defparameter *location* 'living-room)

(defun look (&key (location *location*) (nodes *nodes*) (edges *edges*) (objects *objects*) (object-locations *object-locations*))
  ;; append = like Clojure concat, for lists.
  (append (describe-location location nodes)
          (describe-paths location edges)
          (describe-objects location objects object-locations)))

(defun walk (direction &key (location *location*) (edges *edges*))
  ;; find = search for an element of a sequence.
  (let ((next (find direction
                    (cdr (assoc location edges))
                    :key #'cadr)))
    (if next
        (progn (setf *location* (car next))
               (look))
        '(you cannot go that way.))))

(defun pickup (object)
  ;; member - checks to see if a particular item is found in a list. Returns (item . rest-of-list)
  ;; (member 2 '(1 2 3)) => (2 3)
  ;;
  ;; (push item place) - mutates list at place by prepending item to it; returns updated list. Convenience for (setf place (cons item place))
  ;; PUSHing a new entry onto the front of an association list is a common CL idiom; functions like assoc return the
  ;; first matching pair, so it doesn't matter if the original value is still somewhere in the alist.
  (cond
    ((member object (objects-at *location* *objects* *object-locations*))
     (push (list object 'body) *object-locations*)
     `(you are now carrying the ,object))
    (t
     `(you cannot get that.))))

(defun inventory ()
  (cons 'items- (objects-at 'body *objects* *object-locations*)))

;;; Chapter 6

;; print = print a value w/ newline afterwards. print always prints
;; objects in such a way that they can always be "read" back into
;; their internal representation.

;; prin1 = print a value with no newline

;; A CHARACTER looks like #\A. #\newline, #\tab, and #\space are special literals

;; Symbols are NORMALLY blind to case, but you can create a case
;; sensitive symbol by surrounding it with pipes:
;;
;; |CaseSensitiveSymbol|
;;
;; Symbols surrounded by pipes can also contain spaces and
;; punctuation.

;; princ prints stuff in a way that is appealing to humans.

;; read = reads in input
;; read-line = reads in input as a string

(defun game-read ()
  ;; read-from-string works just like read, but reads from a string instead of the standard input
  ;;
  ;; concatenate concatenates strings together.
  (let ((cmd (read-from-string
              (concatenate 'string "(" (read-line) ")"))))
    (flet ((quote-it (x)
             (list 'quote x)))
      (cons (car cmd) (mapcar #'quote-it (cdr cmd))))))

(defparameter *allowed-commands* '(look walk pickup inventory))

(defun game-eval (form)
  (if (member (car form) *allowed-commands*)
      (eval form)
      '(i do not know that command.)))

(defun tweak-text (lst caps lit)
  (when lst
    (let ((item (car lst))
          (rest (cdr lst)))
      (cond
        ((eq item #\space)
         (cons item (tweak-text rest caps lit)))

        ((member item '(#\! #\? #\.))
         (cons item (tweak-text rest t lit)))

        ((eq item #\")
         (tweak-text rest caps (not lit)))

        (lit
         (cons item (tweak-text rest nil lit)))

        ((or caps lit)
         (cons (char-upcase item) (tweak-text rest nil lit)))

        (t
         (cons (char-downcase item) (tweak-text rest nil nil)))))))

(defun game-print (object)
  ;; prin1-to-string = convert to a string without adding newlines/spaces
  ;; string-trim =
  ;; coerce = coerce an object to a type. (String to list of characters in this case)
  (princ (coerce (tweak-text (coerce (string-trim "() " (prin1-to-string object))
                                     'list)
                             t
                             nil)
                 'string))
  (fresh-line))

(defun game-repl ()
  (let ((cmd (game-read)))
    (print cmd)
    (unless (eq (car cmd) 'quit)
      (game-print (game-eval cmd))
      (game-repl))))
