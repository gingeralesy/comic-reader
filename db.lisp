(in-package #:org.gingeralesy.web.comic-reader)

(define-trigger db:connected ()
  "Creates the database tables if they don't already exist."
  (db:create 'comic '((comic-id (:varchar 16))
                      (comic-name (:varchar 128))
                      (author (:varchar 64))
                      (cover-uri (:varchar 128))
                      (description :text)
                      (read-direction (:integer 1))
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

(defun comic (&optional comic-id)
  (dm:get-one 'comic
              (if (or* comic-id)
                  (db:query (:= 'comic-id comic-id))
                  (db:query (:= 'is-default 1)))))

(defun page (comic-id &optional page-number (up-to-time (get-universal-time)))
  (dm:get-one 'comic-page
              (if page-number
                  (db:query (:and (:= 'comic-id comic-id)
                                  (:= 'page-number page-number)
                                  (:<= 'publish-time up-to-time)))
                  (db:query (:and (:= 'comic-id comic-id)
                                  (:<= 'publish-time up-to-time))))
              :sort '((publish-time :DESC))))
