(in-package #:org.gingeralesy.web.comic-reader)

(admin:define-panel create-comic comic-reader
    (:access (perm comic-reader admin comic)) :tooltip "Manage your comics." :lquery (template "admin-create-comic.ctml")
  (with-actions (error info)
      ((:save
        (setf (config-tree :comic :title) (post-var "title")
              (config-tree :comic :description) (post-var "description")
              (config-tree :comic :cover-uri) (post-var "cover-uri")
              (config-tree :comic :is-default) (post-var "is-default"))))
    (r-clip:process
     T :error error :info info)))
