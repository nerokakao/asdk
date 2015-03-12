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
  "return '(:A NIL :B NIL) if from path /path/:a/:b"
  (let ((matches (mapcar #'read-from-string (get-param-names path)))
	(dummy '())
	(res '()))
    (dolist (m matches)
      (push nil dummy))
    (mapc #'(lambda (&rest e) (setf res (append res e))) matches dummy)
    res))

(defun make-path-scanner (path params-matches)
  "input /path/:a '(A `\\d+')  output(as string) ^/path/([^/]+)$"
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
  "eg: input '(get `/path/:a' fn :a `\\d+')"
  (destructuring-bind (http-type path fn-handler &rest params-matches) template
    (make-route
     :http-type (read-from-string (concatenate 'string ":" (string http-type)))
     :path path
     :path-scanner (make-path-scanner path params-matches)
     :fn-handler fn-handler)))
  
(defun match-handler (route request)
  (let ((params (get-param-matches (route-path route))))
    (multiple-value-bind (match results)
	(cl-ppcre:scan-to-strings (route-path-scanner route) (tbnl:script-name request))
      (when results
	(loop
	     for v in (coerce results 'list)
	     for (p vv) on params by #'cddr
	     do (setf (getf params p) v)))
      (if (or (and match t (eq (request-method request) :HEAD))
	      (and match (eq (route-http-type route) (request-method request))))
	  (return-from match-handler (list (route-fn-handler route) params))
	  (return-from match-handler (list nil nil))))))

(defclass ht-acceptor (tbnl:acceptor)
  ())

(defun defunction (func)
  (if (fboundp func)
      func
      #'(lambda (&rest body) (format nil "Function Handler not implemented! ~A" body))))

(defmethod tbnl:acceptor-dispatch-request ((acceptor ht-acceptor) request)
  (loop for route in *routes*
       for (action params) = (match-handler route request)
       when action return (if params
			      (funcall (defunction action) params)
			      (funcall (defunction action)))
       finally (call-next-method)))

(defun add-route (template)
  (push (gen-route template) *routes*))

#+test
(defvar *server* (make-instance 'ht-acceptor :port 4242))

#+test
(tbnl:start *server*)

#+test
(progn
  (pop *routes*)
  (add-route '(get "/index/:a/:b" show-inde :a "\\d+" :b "\\d+"))

  (defun show-index (params)
    (let ((num (getf params :num)))
      (format t "~A~%" params)
      num)))



#+test
(loop
     for i in '(1 2 3)
     )

#+test
(multiple-value-bind (a b) '(2 2)
  (format t "~%~A ~A~%" a b))

#+test
(get-param-matches "/path/:a/:b")

#+test
(get-param-names "/path/:a/:b")
    
#+test
(make-path-scanner "/path/:a" '(A "\\d+"))

#+test
(mapc #'(lambda (&rest e) (format t ">>~A~%" e)) '(1 2 6) '(3 4 5) '("a"))

#+test
(cl-ppcre:create-scanner (format nil "^~A$" "/path/:a/:b"))

#+test
(gen-route '(get "/path/:a" fn :a "\\d+"))

#+test
(gen-route '(get "/path/:para1" fn))

#+test
(destructuring-bind (http-type path fn-handler &rest params-matches) '(get "/path/:a" fn :a "\\d+")
  (format t "~%~A~%" params-matches)
  (make-path-scanner path params-matches))

#+test
(destructuring-bind (http-type path fn-handler &rest params-matches) '(get "/path" fn)
  (make-path-scanner path params-matches))
