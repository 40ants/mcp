(uiop:define-package #:40ants-mcp/server/errors
  (:use #:cl)
  (:import-from #:serapeum
                #:soft-list-of)
  (:export #:tool-error-content
           #:tool-error))
(in-package #:40ants-mcp/server/errors)


(define-condition tool-error ()
  ((content :initarg :content
            :type (or content
                      (soft-list-of content))
            :reader tool-error-content))
  (:documentation "You should signal this error in case if the tool can't accomplish it's job.")
  (:report (lambda (condition stream)
             (format stream "~A"
                     (tool-error-content condition)))))
