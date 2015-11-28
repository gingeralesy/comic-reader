(in-package #:org.gingeralesy.web.comic-reader)

(define-trigger db:connected ()
  "Creates the database tables if they don't already exist."
  (db:create 'comic '((comic-path (:varchar 16))
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

(defun set-comic (comic-path comic-name author cover-uri
                  description &key read-direction is-default comic-id)
  "Adds a new comic to the database or updates an old one."
  (let* ((default-comic (comic))
         (old-comic (comic :path comic-path))
         (read-direction (if (and read-direction (eql read-direction :left))
                             1 0))
         (is-default (if (or is-default (not default-comic))
                         1 0))
         (field-values (alexandria:plist-hash-table `(:comic-path ,comic-path
                                                      :comic-name ,comic-name
                                                      :author ,author
                                                      :cover-uri ,cover-uri
                                                      :description ,description
                                                      :read-direction ,read-direction
                                                      :is-default ,is-default))))
    (when (= 1 is-default)
      (when default-comic
        (db:update 'comic (db:query (:= 'is-default 1))
                   (alexandria:plist-hash-table '(:is-default 0)))))
    (if old-comic
        (db:update 'comic (db:query (:= 'comic-path comic-path)) field-values)
        (db:insert 'comic field-values))))

(defun comic (&key path id)
  "Gets a specific or the default comic."
  (let ((path (or* path)))
    (dm:get-one 'comic
                (cond
                  ((and id path) (db:query (:and (:= '_id id) (:= 'comic-path path))))
                  (id (db:query (:= '_id id)))
                  (path (db:query (:= 'comic-path path)))
                  (T (db:query (:= 'is-default 1)))))))

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
