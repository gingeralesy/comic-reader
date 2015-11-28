(in-package #:org.gingeralesy.web.comic-reader)

(admin:define-panel create-comic comic-reader (:access (perm admin comic)
                                               :tooltip "Manage your comics."
                                               :lquery (template "admin-create-comic.ctml"))
  (with-actions (error info)
      ((:save
        (let ((comic-id (post-var "comic-id"))
              (comic-name (post-var "comic-name"))
              (cover-uri (or* (post-var "cover-uri")))
              (description (or* (post-var "description")))
              (is-default (when (or* (post-var "is-default"))
                            (ratify:perform-test :boolean (post-var "is-default"))))
              (author (auth:current)))
          (unless author (error 'api-auth-error :message "Missing user!"))
          (unless (cl-ppcre:scan "^[\\w-_]+$" comic-id)
            (error 'api-argument-invalid :message "Invalid comic URL path."))
          (unless comic-name (error 'api-argument-missing :message "Comic name not provided!"))
          (when (or* cover-uri) (ratify:perform-test :url cover-uri))
          (set-comic comic-id comic-name (user:username author)
                     cover-uri description :is-default is-default))))
    (r-clip:process
     T :error error :info info :comics (comics))))

(admin:define-panel debug-comic comic-reader (:access (perm admin comic)
                                              :tooltip "This is a debug panel."
                                              :lquery (template "admin-debug.ctml"))
  (r-clip:process T))
