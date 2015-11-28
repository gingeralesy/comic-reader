(in-package #:org.gingeralesy.web.comic-reader)

(define-page index #@"comic/" (:lquery (template "main.ctml"))
  "The front page of the site."
  (r-clip:process T))

(define-page comic #@"comic/comic(/([a-zA-Z]+))?(/([0-9]+))?" (:uri-groups (NIL comic-path NIL page-number)
                                                               :lquery (template "comic.ctml"))
  "The web page for displaying a web comic page."
  (let* ((comic (or (comic :path comic-path)
                    (error 'request-not-found :message "Invalid comic requested.")))
         (page (or (page (dm:field comic '_id) (when page-number (parse-integer page-number :junk-allowed T)))
                   (error 'request-not-found :message "Comic page does not exist."))))
    (r-clip:process
     T
     :comic-path (dm:field comic 'comic-path)
     :page-number (dm:field page 'page-number))))

(define-api comic/page (comic-path page-number) ()
  "API interface for getting metadata for a comic page."
  (case (http-method *request*)
    (:get
     (let* ((comic (comic :path (or* comic-path)))
            (page-number (when (or* page-number) (parse-integer page-number :junk-allowed T)))
            (page (page (dm:field comic '_id) :page-number page-number)))
       (unless (and page (<= (dm:field page 'publish-time) (get-universal-time)))
         (error 'request-not-found :message "Comic page does not exist."))
       (api-output page)))
    (T (wrong-method-error (http-method *request*)))))
