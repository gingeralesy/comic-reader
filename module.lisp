(in-package #:rad-user)
(define-module #:org.gingeralesy.web.comic-reader
  (:nicknames #:comic-reader)
  (:use #:cl #:radiance)
  (:domain "comic"))
(in-package #:org.gingeralesy.web.comic-reader)

;; Globally used functions

(defun wrong-method-error (method)
  "Throws an error with a message informing this is an unsupported request method."
  (error 'api-call-not-found :message (format nil "Request type ~:@(~a~) not supported." method)))

(lquery:define-lquery-function reader-template (node object)
  "Adds content from a different template."
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (template (format NIL "~(~a~).ctml" object)) :root node)
  node)
