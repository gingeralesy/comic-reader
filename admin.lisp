(in-package #:org.gingeralesy.web.comic-reader)

(admin:define-panel create-comic comic-reader (:access (perm author)
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
              (author (if (auth:current) (user:username (auth:current))
                          (error 'api-auth-error :message "Missing user!"))))
          (unless (cl-ppcre:scan "^[\\w-_]+$" comic-path)
            (error 'api-argument-invalid :message "Invalid comic URL path."))
          (unless comic-name (error 'api-argument-missing :message "Comic name not provided!"))
          (when (or* cover-uri) (ratify:perform-test :url cover-uri))
          (set-comic comic-path comic-name (user:username author)
                     cover-uri description :is-default is-default))))
    (r-clip:process
     T :error error :info info :comics (comics (user:username (auth:current))))))

(admin:define-panel manage-comic comic-reader (:access (perm author)
                                               :tooltip "Manage a specific comic."
                                               :lquery (template "admin-manage-comic.ctml"))
  (let* ((comic-id (int-get-var "comic"))
         (comic (when (or* comic-id) (comic :id comic-id)))
         (pages (when comic (pages (dm:field comic '_id) :up-to-time NIL)))
         (author (if (auth:current) (user:username (auth:current))
                     (error 'api-auth-error :message "Missing user!"))))
    (when (and comic (not (string= author (dm:field comic 'author))))
      (error 'api-auth-error :message "You may only manage your own comics."))
    (r-clip:process
     T :comics (comics author)
       :comic comic
       :pages pages)))

(admin:define-panel manage-comic-pages comic-reader (:access (perm author)
                                                     :tooltip "Manage a specific page of a comic."
                                                     :lquery (template "admin-manage-page.ctml"))
  (let* ((comic-id (int-get-var "comic"))
         (comic (when (or* comic-id) (comic :id comic-id)))
         (page-num (int-get-var "page"))
         (pages (when comic (pages (dm:field comic '_id) :up-to-time NIL)))
         (page (when (or* page-num) (page (dm:field comic '_id) :page-number page-num :up-to-time NIL)))
         (author (if (auth:current) (user:username (auth:current))
                     (error 'api-auth-error :message "Missing user!"))))
    (when (and comic (not (string= author (dm:field comic 'author))))
      (error 'api-auth-error :message "You may only manage your own comics."))
    (r-clip:process
     T :comics (comics (user:username (auth:current)))
       :comic comic
       :pages pages
       :page page)))

(defun int-get-var (var-name)
  (when (and var-name (or* (get-var var-name)))
    (parse-integer (get-var var-name) :junk-allowed T)))
