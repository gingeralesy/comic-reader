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
                           (thumb-uri (:varchar 128))
                           (double-page (:integer 1))
                           (width (:integer 8))
                           (height (:integer 8)))))

(defun comics (&optional author)
  "Gets all of the comics. All of them. Or just for an author."
  (dm:get 'comic 
          (if author
              (db:query (:= 'author author))
              (db:query :all))
          :sort '((_id :ASC))))

(defun comic-hash-table (comic)
  "Converts the comic data model into a hash-table."
  (alexandria:plist-hash-table `(:id ,(dm:id comic)
                                 :comic-path ,(dm:field comic 'comic-path)
                                 :comic-name ,(dm:field comic 'comic-name)
                                 :author ,(dm:field comic 'author)
                                 :cover-uri ,(dm:field comic 'cover-uri)
                                 :description ,(dm:field comic 'description)
                                 :read-direction ,(dm:field comic 'read-direction)
                                 :is-default ,(= 1 (dm:field comic 'is-default))
                                 :page-count ,(length (pages (dm:id comic))))))

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

(defun pages (comic-id &key (up-to-time (get-universal-time)) (start 0) count)
  "Gets all of the pages for the specified comic."
  (dm:get 'comic-page
          (if up-to-time
              (db:query (:and (:= 'comic-id comic-id)
                              (:<= start 'page-number)
                              (:<= 'publish-time up-to-time)))
              (db:query (:and (:= 'comic-id comic-id)
                              (:<= start 'page-number))))
          :amount count
          :sort '((page-number :ASC))))

(defun page-hash-table (page)
  "Converts the page data model into a hash-table."
  (alexandria:plist-hash-table `(:id ,(dm:id page)
                                 :comic-id ,(dm:field page 'comic-id)
                                 :page-number ,(dm:field page 'page-number)
                                 :title ,(dm:field page 'title)
                                 :commentary ,(dm:field page 'commentary)
                                 :creation-time ,(dm:field page 'creation-time)
                                 :publish-time ,(dm:field page 'publish-time)                                               
                                 :tags ,(dm:field page 'tags)
                                 :transcript ,(dm:field page 'transcript)
                                 :image-uri ,(dm:field page 'image-uri)
                                 :thumb-uri ,(dm:field page 'thumb-uri)
                                 :double-page ,(= 1 (dm:field page 'double-page))
                                 :width ,(dm:field page 'width)
                                 :height ,(dm:field page 'height))))

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
                 &key title commentary publish-time
                      tags transcript thumb-uri double-page
                      (width 0) (height 0))
  (unless (comic :id comic-id)
    (error 'database-invalid-field :message "Invalid comic specified."))
  (let* ((old-page (page comic-id :page-number page-number))
         (creation-time (if old-page
                            (dm:field old-page 'creation-time)
                            (get-universal-time)))
         (field-values (alexandria:plist-hash-table `(:comic-id ,comic-id
                                                      :page-number ,page-number
                                                      :title ,title
                                                      :commentary ,commentary
                                                      :creation-time ,creation-time
                                                      :publish-time ,(or publish-time
                                                                         creation-time)
                                                      :tags ,tags
                                                      :transcript ,transcript
                                                      :image-uri ,image-uri
                                                      :thumb-uri ,thumb-uri
                                                      :double-page ,(if double-page 1 0)
                                                      :width ,width
                                                      :height ,height))))
    (if old-page
        (db:update 'comic-page
                   (db:query (:and (:= 'comic-id comic-id)
                                   (:= 'page-number page-number)))
                   field-values)
        (db:insert 'comic-page field-values))))
