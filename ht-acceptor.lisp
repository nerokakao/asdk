(in-package :cl-user)

(ql:quickload '("hunchentoot" "cl-ppcre"))

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
  "return param names in a list: (\":para1\" \":para2\") from path(string) \"/path/:para1/:para2\""
  (cl-ppcre:all-matches-as-strings *route-param-scanner* path))

(defun get-param-matches (path)
  (let ((matches (mapcar #'read-from-string (get-param-names path)))
	(dummy '())
	(res '()))
    (dolist (m matches)
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
    (format t "^~A$~%" new-path)
    (cl-ppcre:create-scanner (format nil "^~A$" new-path))))

(defun gen-route (template)
  (destructuring-bind (http-type path fn-handler &rest params-matches) template
    (make-route
     :http-type (read-from-string (concatenate 'string ":" (string http-type)))
     :path path
     :path-scanner (make-path-scanner path params-matches)
     :fn-handler fn-handler)))
  
(defun match-handler (route request)
  ())


    
    
#+test
(mapc #'(lambda (&rest e) (format t ">>~A~%" e)) '(1 2 6) '(3 4 5) '("a"))

#+test
(cl-ppcre:create-scanner (format nil "^~A$" "/path/:a/:b"))

#+test
(gen-route '(get "/path/:a" fn :a "\\d+"))

#+test
(gen-route '(get "/path" fn))

#+test
(destructuring-bind (http-type path fn-handler &rest params-matches) '(get "/path/:a" fn :a "\\d+")
  (make-path-scanner path params-matches))

#+test
(destructuring-bind (http-type path fn-handler &rest params-matches) '(get "/path" fn)
  (make-path-scanner path params-matches))
