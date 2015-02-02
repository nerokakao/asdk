(in-package :cl-user)
;;;;require: quicklisp has been installed

;;;;require start
(ql:quickload :hunchentoot) ;web server
(ql:quickload :xmls)        ;simple xml parse
(ql:quickload :postmodern)  ;database driver
;;;;require end

(defpackage :asdk-asd
  (:use :cl
	:asdf))

(in-package :asdk-asd)

(defsystem "asdk"
  :serial t
  :version "1.0.0"
  :author "nero <nero.tan@daumkakao.com>"
  :description "daumkakao attendance system"
  :depends-on (:postmodern
	       :hunchentoot
	       :xmls)
  :components ((:file "packages")
	       (:file "db")
	       (:file "www")
	       (:file "controller")
	       (:file "routes")))

