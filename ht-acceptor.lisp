(in-package :cl-user)

(ql:quickload '("hunchentoot"))

(defpackage #:test
  (:use :cl :hunchentoot :cl-ppcre))

(in-package :test)

(defstruct route
  http-type
  path
  path-scanner
  fn-handler)

;; list of routes struct
(defvar *routes* '())

(defparameter *route-param-scanner* (cl-ppcre:create-scanner "(:[^\\W]+)"))
(defparameter *default-param-regex* "([^\/]+)")

(defun get-param-names (path)
  (cl-ppcre:all-matches-as-strings *route-param-scanner* path))

(defun get-param-matches (path)
  (let ((matches (mapcar #'read-from-string (get-param-names path)))
	(dummy '())
	(res '()))
    (dolist (m matches)
      (format t "~A~%" m)
      (push nil dummy))
    (mapc #'(lambda (&rest e) (setf res (append res e))) matches dummy)
    res))

(defun make-path-scanner (path params-matches)
  (let* ((new-path path)
	 (param-names (get-param-names path))
	 (pattern *default-param-regex*))
    (if param-names
	(dolist (p param-names)
	  (let ((it (getf params-matches (read-from-string p))))
	    (if it
		(setf new-path
		      (cl-ppcre:regex-replace p new-path (format nil "(~A)" it)))
		(setf new-path
		      (cl-ppcre:regex-replace p new-path pattern))))))
    (cl-ppcre:create-scanner (format nil "^~A$" new-path))))

(defun gen-route (template)
  (destructuring-bind (http-type path fn-handler &rest params-matches) template
    (make-route
     :http-type (read-from-string (concatenate 'string ":" (string http-type)))
     :path path
     :path-scanner (make-path-scanner path params-matches)
     :fn-handler fn-handler)))
  
(defun match-handler (route request)
  ()
