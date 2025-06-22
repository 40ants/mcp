(uiop:define-package #:40ants-mcp/content/base
  (:use #:cl)
  (:export #:content
           #:content-type))
(in-package #:40ants-mcp/content/base)


(defclass content ()
  ((type :type string
         :initform "unknown"
         :initarg :type
         :reader content-type)))


(defmethod print-object ((obj content) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "type: ~A"
            (content-type obj))))
