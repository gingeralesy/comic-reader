(in-package #:org.gingeralesy.web.comic-reader)

(define-page index #@"comic/" (:lquery (template "main.ctml"))
  "The front page of the site."
  (r-clip:process T))

(define-page comic #@"comic/comic(/([a-zA-Z]+))?(/([0-9]+))?" (:uri-groups (NIL comic-id NIL page-number)
                                                               :lquery (template "comic.ctml"))
  "The web page for displaying a web comic page."
  (let* ((comic (or (comic comic-id)
                    (error 'request-not-found :message "Invalid comic requested.")))
         (page (page (comic-id comic) (when page-number (parse-integer page-number :junk-allowed T)))))
    (r-clip:process
     T
     :comic-id (comic-id comic)
     :page-number (page-number page))))

(define-api comic/page (comic-id page-number) ()
  "API interface for getting metadata for a comic page."
  (let ((comic-id comic-id)
        (page-number (when page-number (parse-integer page-number :junk-allowed T))))
    (case (http-method *request*)
      (:get
       (let ((page (page comic-id page-number)))
         (unless (and page (<= (publish-time page) (get-universal-time)))
           (error 'request-not-found :message "Comic page does not exist."))
         (api-output page)))
      (T (wrong-method-error (http-method *request*))))))
