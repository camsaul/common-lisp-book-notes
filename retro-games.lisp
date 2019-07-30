;; (ql:quickload '(cl-who hunchentoot parenscript))

(defpackage :retro-games
  (:use :cl :cl-who :hunchentoot :parenscript))

;; (in-package :retro-games)

(defclass game ()
  ((name :reader name
         :initarg :name)
   (votes :accessor votes
          :initform 0)))

(defmethod print-object ((object game) stream)
  (print-unreadable-object (object stream :type t)
    (with-slots (name votes) object
      (format stream "name: ~s with ~d votes" name votes))))

(defvar *games* '())

(defun game-from-name (name)
  (find name *games* :test #'string-equal :key #'name))

(defun games ()
  (sort (copy-list *games*) #'> :key #'votes))

(defun add-game (name)
  (unless (game-from-name name)
    (push (make-instance 'game :name name) *games*)))

(defun vote-for (game)
  (incf (votes game)))

;; (setf (html-mode) :html5)
;;
;; (with-html-output (*standard-output* nil :prologue t :indent t)
;;   (:html
;;    (:head
;;     (:title "Test page"))
;;    (:body
;;     (:p "CL-WHO is really easy to use"))))

(setf (html-mode) :html5)

(defmacro standard-page ((&key title) &body body)
  `(with-html-output-to-string (*standard-output* nil :prologue t :indent t)
     (:html :lang "en")
     (:head
      (:meta :charset "utf-8")
      (:title ,title)
      (:link :type "text/css"
             :rel "stylesheet"
             :href "/retro.css"))
     (:body
      (:div :id "header"
            (:img :src "/logo.jpg"
                  :alt "Commodore 64"
                  :class "logo")
            (:span :class "strapline"
                   "Vote on your favorite Retro Game"))
      ,@body)))

(defun start-server (port)
  (start (make-instance 'easy-acceptor :port port)))

(define-easy-handler (retro-games :uri "/retro-games") ()
  (standard-page (:title "Retro Games")
    (:h1 "Vote on your all-time favorite retro games!")
    (:p "Missing a game? Make it available for votes "
        (:a :href "new-game" "here"))
    (:h2 "Current stand")
    (:div :id "chart"
          (:ol
           (dolist (game (games))
             (htm
              (:li (:a :href (format nil "vote?name=~a" (url-encode (name game)))
                       "Vote!"))
              (fmt "~A with ~d votes" (escape-string (name game)) (votes game))))))))

(define-easy-handler (vote :uri "/vote") (name)
  (let ((game (game-from-name name)))
    (when game
      (vote-for game)))
  (redirect "/retro-games"))

(define-easy-handler (new-game :uri "/new-game") ()
  (standard-page (:title "Add a new game")
    (:h1 "Add a new game to the chart")
    (:form :action "/game-added" :method "post" :id "addform"
           (:p "What is the name of the game?" (:br)
               (:input :type "text" :name "name" :class "txt"))
           (:p (:input :type "submit" :value "Add" :class "btn")))))

(define-easy-handler (game-added :uri "/game-added") (name)
  (unless (or (null name) (zerop (length name)))
    (add-game name))
  (redirect "/retro-games"))
