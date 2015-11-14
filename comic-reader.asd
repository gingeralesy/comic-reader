(in-package #:cl-user)
(asdf:defsystem #:comic-reader
  :defsystem-depends-on (:radiance :lass)
  :class "radiance:module"
  :components ((:file "comic-reader")
               (:lass-file "static/css/style"))
  :depends-on (:r-clip :i-json))
