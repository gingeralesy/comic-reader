(in-package #:rad-user)
(define-module #:comic-reader
  (:use #:cl #:radiance))
(in-package #:comic-reader)

(remove-uri-dispatcher 'welcome)

;; Triggers
(define-trigger db:connected ()
  (db:create 'comic '((comic-id (:varchar 16))
                      (comic-name (:varchar 128))
                      (author (:varchar 64))
                      (description :text)
                      (is-default (:integer 1))))
  (db:create 'comic-page '((comic-id :id)
                           (page-number (:integer 4))
                           (title (:varchar 128))
                           (commentary :text)
                           (time (:integer 8))
                           (tags :text)
                           (transcript :text)
                           (image-uri (:varchar 128))
                           (thumb-uri (:varchar 128)))))

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

(define-page admin #@"/admin" (:lquery (template "admin.ctml"))
  (r-clip:process T)) ;; TODO: authentication stuffs

;; User API

(define-api comic/page (comic-id page-number) ()
  (let ((comic-id (or* comic-id "default"))
        (page-number (or* page-number "latest")))
    (case (http-method *request*)
      (:get
       (api-output
        (dm:get-one 'comic-page (db:query (:and (:= 'comic-id comic-id)
                                                (:= 'page-number page-number))))))
      (T (error (format nil "Request type ~(~a~) not supported." (http-method *request*)))))))

;; Admin API

(define-api admin/comic/page () () ;; TODO: authentication checks here
  (api-output
   (alexandria:plist-hash-table
    '(:teapot "short and stout"))))

;; Other functions

(lquery:define-lquery-function reader-template (node object)
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (template (format NIL "~(~a~).ctml" object)) :root node)
  node)
