(in-package #:org.gingeralesy.web.comic-reader)

(define-page admin #@"comic/admin" (:lquery (template "admin.ctml"))
  "The admin console user interface."
  (r-clip:process T)) ;; TODO: authentication stuffs

(define-api admin/comic/page () () ;; TODO: authentication checks here
  "API interface for adding metadata for a new page to a comic."
  (case (http-method *request*)
    (:post
     (api-output
      (alexandria:plist-hash-table
       '(:teapot "short and stout"))))
    (T (wrong-method-error (http-method *request*)))))
