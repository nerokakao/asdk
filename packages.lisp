;;;;devel-env: sbcl

(in-package :cl-user)

(defpackage #:asdk
  (:nicknames #:dk)
  (:use :cl
	:hunchentoot
	:xmls
	:postmodern))
