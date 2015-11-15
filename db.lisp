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

(defun set-comic (comic-id comic-name author cover-uri
                  description &key read-direction is-default)
  "Adds a new comic to the database or updates an old one."
  (let* ((read-direction (if (and read-direction
                                  (or (string= read-direction "left")
                                      (eql read-direction :left)))
                             1 0))
         (is-default (if (or* is-default) 1 0))
         (old-comic (comic comic-id))
         (field-values (alexandria:plist-hash-table `(:comic-id ,comic-id
                                                      :comic-name ,comic-name
                                                      :author ,author
                                                      :cover-uri ,cover-uri
                                                      :description ,description
                                                      :read-direction ,read-direction
                                                      :is-default ,is-default))))
    (when (= 1 is-default)
      (let ((default-comic (comic)))
        (when default-comic
          (db:update 'comic (db:query (:= 'is-default 1))
                     (alexandria:plist-hash-table `(:is-default 0))))))
    (if old-comic
        (db:update 'comic (db:query (:= 'comic-id comic-id)) field-values)
        (db:insert 'comic field-values))))

(defun comic (&optional comic-id)
  "Gets a specific or the default comic."
  (dm:get-one 'comic
              (if (or* comic-id)
                  (db:query (:= 'comic-id comic-id))
                  (db:query (:= 'is-default 1)))))

(defun comics ()
  "Gets all of the comics. All of them."
  (dm:get 'comic (db:query :all)))

(defun page (comic-id &optional page-number (up-to-time (get-universal-time)))
  "Gets the specific page of a comic or the latest one."
  (dm:get-one 'comic-page
              (if page-number
                  (db:query (:and (:= 'comic-id comic-id)
                                  (:= 'page-number page-number)
                                  (:<= 'publish-time up-to-time)))
                  (db:query (:and (:= 'comic-id comic-id)
                                  (:<= 'publish-time up-to-time))))
              :sort '((publish-time :DESC))))
