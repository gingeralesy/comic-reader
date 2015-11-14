(in-package #:rad-user)
(define-module #:comic-reader
  (:use #:cl #:radiance))
(in-package #:comic-reader)

(remove-uri-dispatcher 'welcome)

;; Pages
(define-page index #@"/" (:lquery (template "main.ctml"))
  (r-clip:process T))

(define-page comic #@"/comic(/[a-zA-Z]+)?(/[0-9]+)?" (:uri-groups (comic-id page-number)
                                                     :lquery (template "comic.ctml"))
  (r-clip:process
   T
   :comic-id (if (< 0 (length comic-id))
                 (subseq comic-id 1 (length comic-id))
                 "default") ;; get default from somewhere
   :page-number (if (< 0 (length page-number))
                    (subseq page-number 1 (length page-number))
                    "0"))) ;; latest page default

;; APIs

(define-api comic/page (comic-id page-number) ()
  (api-output
   (alexandria:plist-hash-table
    `(:success T))))

;; Other functions

(lquery:define-lquery-function reader-template (node object)
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (template (format NIL "~(~a~).ctml" object)) :root node)
  node)
