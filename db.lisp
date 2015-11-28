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

(defun comics (&optional author)
  "Gets all of the comics. All of them. Or just for an author."
  (dm:get 'comic 
          (if author
              (db:query (:= 'author author))
              (db:query :all))
          :sort '((_id :ASC))))

(defun comic (&key path id)
  "Gets a specific or the default comic."
  (let ((path (or* path)))
    (dm:get-one 'comic
                (cond
                  ((and id path) (db:query (:and (:= '_id id) (:= 'comic-path path))))
                  (id (db:query (:= '_id id)))
                  (path (db:query (:= 'comic-path path)))
                  (T (db:query (:= 'is-default 1)))))))

(defun set-comic (comic-path comic-name author cover-uri
                  description &key read-direction is-default comic-id)
  "Adds a new comic to the database or updates an old one."
  (let* ((default-comic (comic))
         (old-comic (if comic-id (comic :id comic-id) (comic :path comic-path)))
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
    (when (and comic-id (not old-comic))
      (error 'database-invalid-field :message "Invalid comic specified."))
    (when (= 1 is-default)
      (when default-comic
        (db:update 'comic (db:query (:= 'is-default 1))
                   (alexandria:plist-hash-table '(:is-default 0)))))
    (if old-comic
        (db:update 'comic (db:query (:= '_id (dm:field old-comic '_id))) field-values)
        (db:insert 'comic field-values))))

(defun pages (comic-id &key (up-to-time (get-universal-time)))
  "Gets all of the pages for the specified comic."
  (dm:get 'comic-page
          (if up-to-time
              (db:query (:and (:= 'comic-id comic-id)
                              (:<= 'publish-time up-to-time)))
              (db:query (:= 'comic-id comic-id)))
          :sort '((publish-time :DESC))))

(defun page (comic-id &key page-number (up-to-time (get-universal-time)))
  "Gets the specific page of a comic or the latest one."
  (dm:get-one 'comic-page
              (cond
                ((and page-number up-to-time)
                 (db:query (:and (:= 'comic-id comic-id)
                                 (:= 'page-number page-number)
                                 (:<= 'publish-time up-to-time))))
                (up-to-time
                 (db:query (:and (:= 'comic-id comic-id)
                                 (:<= 'publish-time up-to-time))))
                (T (db:query (:= 'comic-id comic-id))))
              :sort '((publish-time :DESC))))

(defun set-page (comic-id page-number image-uri
                 &key title commentary (publish-time (get-universal-time))
                      tags transcript thumb-uri)
  (unless (comic :id comic-id) (error 'database-invalid-field :message "Invalid comic specified."))
  (let ((old-page (page comic-id :page-number page-number))
        (field-values (alexandria:plist-hash-table `(:comic-id ,comid-id
                                                     :page-number ,page-number
                                                     :title ,title
                                                     :commentary ,commentary
                                                     :creation-time ,(get-universal-time)
                                                     :publish-time ,publish-time
                                                     :tags ,tags
                                                     :transcript ,transcript
                                                     :image-uri ,image-uri
                                                     :thumb-uri ,thumb-uri))))
    (if old-page
        (db:update 'comic-page
                   (db:query (:and (:= 'comic-id comic-id)
                                   (:= 'page-number page-number)))
                   field-values)
        (db:insert 'comic-page field-values))))
