(in-package #:org.gingeralesy.web.comic-reader)

(define-page index #@"comic/" (:lquery (template "main.ctml"))
  "The front page of the site."
  (r-clip:process T))

(define-page comic #@"comic/comic(/([a-zA-Z]+))?(/([0-9]+))?"
    (:uri-groups (NIL comic-path NIL page-number)
     :lquery (template "comic.ctml"))
  "The web page for displaying a web comic page."
  (let* ((comic (or (comic :path comic-path)
                    (error 'request-not-found :message "Invalid comic requested.")))
         (page (or (page (dm:field comic '_id)
                         :page-number (when (or* page-number)
                                        (parse-integer page-number :junk-allowed T)))
                   (error 'request-not-found :message "Comic page does not exist."))))
    (r-clip:process
     T
     :comic comic
     :page page)))

(define-api comic/page (comic-id page-number) ()
  "API interface for getting metadata for a comic page."
  (case (http-method *request*)
    (:get
     (let* ((comic (comic :id (when (or* comic-id) (parse-integer comic-id :junk-allowed T))))
            (page-number (when (or* page-number) (parse-integer page-number :junk-allowed T)))
            (page (page (dm:field comic '_id) :page-number page-number)))
       (unless (and page (<= (dm:field page 'publish-time) (get-universal-time)))
         (error 'request-not-found :message "Comic page does not exist."))
       (api-output page)))
    (T (wrong-method-error (http-method *request*)))))

(lquery:define-lquery-function image-metadata (node comic page)
  "Adds the attribute fields to an img element to display a comic page."
  (lquery:$ node
    (attr :src (dm:field page :image-uri)
          :title (or* (dm:field page :title)
                      (format NIL "~a - Page ~a"
                              (dm:field comic :comic-name)
                              (dm:field page :page-number)))))
  node)
