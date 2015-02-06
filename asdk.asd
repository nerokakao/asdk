(in-package :cl-user)
;;;;require start
(ql:quickload :hunchentoot)
(ql:quickload :xmls)
(ql:quickload :postmodern)
;;;;require end

(defpackage :asdk-asd
  (:use :cl
	:asdf))

(in-package :asdk-asd)

(defsystem "asdk"
  :serial t
  :version "1.0.0"
  :author "nero <nero.tan@daumkakao.com>"
  :description "daumkakao system"
  :depends-on (:postmodern
	       :hunchentoot
	       :xmls)
  :components ((:file "packages")
	       (:file "db")
	       (:file "www")
	       (:file "routes-func")
	       (:file "routes")))

