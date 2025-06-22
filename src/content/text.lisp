(uiop:define-package #:40ants-mcp/content/text
  (:use #:cl)
  (:import-from #:40ants-mcp/content/base
                #:content
                #:content-type)
  (:export #:text-content
           #:content-text))
(in-package #:40ants-mcp/content/text)


(defclass text-content (content)
  ((text :type string
         :initarg :text
         :reader content-text))
  (:default-initargs :type "text"))


(defmethod print-object ((obj text-content) stream)
  (print-unreadable-object (obj stream :type t)
    (format stream "type: ~A, text: ~A"
            (content-type obj)
            (content-text obj))))
