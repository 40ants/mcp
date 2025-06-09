(uiop:define-package #:40ants-mcp
  (:use #:cl)
  (:nicknames #:40ants-mcp/core)
  (:export #:hello
           #:make-hello
           #:say
           #:user-name))
(in-package #:40ants-mcp)


(defclass hello ()
  ((name :initarg :name
         :reader user-name))
  (:documentation "Example class."))


(defun make-hello (name)
  "Makes hello world example"
  (make-instance 'hello
                 :name name))


(defgeneric say (obj)
  (:documentation "Say what should be said.")
  (:method ((obj hello))
    (format nil "Hello, ~A!~%"
            (user-name obj))))
