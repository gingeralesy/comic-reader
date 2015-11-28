(in-package #:org.gingeralesy.web.comic-reader)

(admin:define-panel create-comic comic-reader (:access (perm admin author)
                                               :tooltip "Manage your comics."
                                               :lquery (template "admin-create-comic.ctml"))
  (with-actions (error info)
      ((:save
        (let ((comic-path (post-var "comic-path"))
              (comic-name (post-var "comic-name"))
              (cover-uri (or* (post-var "cover-uri")))
              (description (or* (post-var "description")))
              (is-default (when (or* (post-var "is-default"))
                            (string= "true" (ratify:perform-test :boolean (post-var "is-default")))))
              (author (auth:current)))
          (unless author (error 'api-auth-error :message "Missing user!"))
          (unless (cl-ppcre:scan "^[\\w-_]+$" comic-path)
            (error 'api-argument-invalid :message "Invalid comic URL path."))
          (unless comic-name (error 'api-argument-missing :message "Comic name not provided!"))
          (when (or* cover-uri) (ratify:perform-test :url cover-uri))
          (set-comic comic-path comic-name (user:username author)
                     cover-uri description :is-default is-default))))
    (r-clip:process
     T :error error :info info :comics (comics))))

(admin:define-panel manage-comic comic-reader (:access (perm admin author)
                                               :tooltip "Manage a specific comic."
                                               :lquery (template "admin-manage-comic.ctml"))
  (let ((comic-id (get-var "id")))
    (r-clip:process
     T :comics (comics) :comic (when (or* comic-id) (comic :id (parse-integer comic-id))))))
