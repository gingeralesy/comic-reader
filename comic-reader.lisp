(in-package #:rad-user)
(define-module #:comic-reader
  (:use #:cl #:radiance))
(in-package #:comic-reader)

(remove-uri-dispatcher 'welcome)

;; Triggers
(define-trigger db:connected ()
  "Creates the database tables if they don't already exist."
  (db:create 'comic '((comic-id (:varchar 16))
                      (comic-name (:varchar 128))
                      (author (:varchar 64))
                      (description :text)
                      (is-default (:integer 1))))
  (db:create 'comic-page '((comic-id :id)
                           (page-number (:integer 4))
                           (title (:varchar 128))
                           (commentary :text)
                           (creation-time (:integer 8))
                           (publish-time (:integer 8))
                           (tags :text)
                           (transcript :text)
                           (image-uri (:varchar 128))
                           (thumb-uri (:varchar 128)))))

;; Pages
(define-page index #@"/" (:lquery (template "main.ctml"))
  "The front page of the site."
  (r-clip:process T))

(define-page comic #@"/comic(/[a-zA-Z]+)?(/[0-9]+)?" (:uri-groups (comic-id page-number)
                                                      :lquery (template "comic.ctml"))
  "The web page for displaying a web comic page."
  (r-clip:process
   T
   :comic-id (if (< 0 (length comic-id))
                 (subseq comic-id 1 (length comic-id))
                 "default") ;; get default from somewhere
   :page-number (if (< 0 (length page-number))
                    (subseq page-number 1 (length page-number))
                    "0"))) ;; latest page default

(define-page admin #@"/admin" (:lquery (template "admin.ctml"))
  "The admin console user interface."
  (r-clip:process T)) ;; TODO: authentication stuffs

;; User API

(define-api comic/page (comic-id page-number) ()
  "API interface for getting metadata for a comic page."
  (let ((comic-id (or* comic-id "default"))
        (page-number (parse-integer page-number))
        (time-now (get-universal-time)))
    (case (http-method *request*)
      (:get
       (let ((page (dm:get-one 'comic-page
                               (if (not page-number)
                                   (db:query (:and (:= 'comic-id comic-id)
                                                   (:<= 'publish-time time-now)))
                                   (db:query (:and (:= 'comic-id comic-id)
                                                   (:= 'page-number page-number)
                                                   (:<= 'publish-time time-now)))))
                               :sort 'publish-time :DESC)))
         (when (or (not page) (> (publish-time page) time-now))
           (error 'request-not-found :message "Comic page does not exist."))
         (api-output page)))
      (T (wrong-method-error (http-method *request*))))))

;; Admin API

(define-api admin/comic/page () () ;; TODO: authentication checks here
  "API interface for adding metadata for a new page to a comic."
  (case (http-method *request*)
    (:post
     (api-output
      (alexandria:plist-hash-table
       '(:teapot "short and stout"))))
    (T (wrong-method-error (http-method *request*)))))

;; Other functions

(defun wrong-method-error (method)
  "Throws an error with a message informing this is an unsupported request method."
  (error 'api-call-not-found :message (format nil "Request type ~(~a~) not supported." method)))

(lquery:define-lquery-function reader-template (node object)
  "Adds content from a different template."
  (setf (plump:children node) (plump:make-child-array))
  (plump:parse (template (format NIL "~(~a~).ctml" object)) :root node)
  node)
