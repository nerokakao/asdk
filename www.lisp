(in-package :asdk)

(defclass dk-acceptor (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "List of dispatch functions for this acceptor"))
  (:documentation
   "This is the acceptor of the ``daumkakao``
   hunchentoot framework"))

(defmethod acceptor-dispatch-request ((acceptor dk-acceptor) request)
  "The easy request dispatcher which selects a request handler
   based on a list of individual request dispatchers all of which can
   either return a handler or neglect by returning NIL."
  (loop for dispatcher in (dispatch-table acceptor)
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))

(defparameter *host* (make-instance 'dk-acceptor :port 4242))

(defun dkstart ()
  (if (not (eql *host* nil))
      (tbnl:start *host*)))


(defun dkstop ()
  (if (not (eql *host* nil))
      (tbnl:stop *host*)))


;;;; route with acceptor
(defvar *dispatch-list* '() "same with tbnl:*dispatch-table*")

(defclass r-acceptor (tbnl:acceptor)
  ((dispatch-table
    :initform '()
    :accessor dispatch-table
    :documentation "same with tbnl:*dispatch-table*"))
  (:documentation "Try to routes request using the function: route"))

(defun str-start-with-colon-p (str)
  (and (> (length str) 1) (eql (elt str 0) #\:)))

(defun last-ele (seq)
  (let ((index (- (length seq) 1)))
    (when (> index 0)
      (elt seq index))))

(defun rm-last-ele (seq)
  (if (> (length seq) 0)
      (subseq seq 0 (1- (length seq)))
      seq))

(defun route-compile (http-req-type uri fn-cb)
  "eg: (route-compile :get ``/a/b/:c`` 'a) ==>> (LIST :GET ``^/a/b/([^/]*)$`` '(C) A)"
  (let* ((uri-to-lst (remove "" (cl-ppcre:split "/" uri) :test #'equal))
	 (start-with-/-p (and (> (length uri) 0) (eql (elt uri 0) #\/)))
	 (end-with-/-p (and (> (length uri) 1) (eql (last-ele uri) #\/)))
	 (eles-with-colon (reverse (reduce #'(lambda (accum nxt)
					       (if (str-start-with-colon-p nxt)
						   (cons nxt accum)
						   accum))
					   uri-to-lst :initial-value '())))
	 (regex-uri (concatenate 'string
				 "^"
				 (when start-with-/-p "/")
				 (rm-last-ele (apply #'concatenate
						     'string
						     (loop for ele in uri-to-lst collect
							  (if (str-start-with-colon-p ele)
							      "([^/]*)/"
							      (concatenate 'string
									   ele
									   "/")))))
				 (when end-with-/-p "/")
				 "$"))
	 (args-to-bind (mapcar #'(lambda (item)
				   (intern (string-upcase (subseq item 1))))
			       eles-with-colon)))
    `(list ,http-req-type ,regex-uri (quote ,args-to-bind) ,fn-cb)))

(defmethod router ((r-actor r-acceptor) req-type req-uri)
  (cl-ppcre:register-groups-bind (uri) ("^([^?]*)\\??.*" req-uri)
    (loop for compiled-route in (dispatch-table r-actor) do
	 (destructuring-bind (requ-type regex-uri args fn-cb) compiled-route
	   (declare (ignore args))
	   (multiple-value-bind (regex str) (cl-ppcre:scan-to-strings regex-uri uri)
	     (declare (ignore regex))
	     (if (and (not (eql str nil))
		      (eql requ-type req-type))
		 (return-from router (apply fn-cb (coerce str 'list)))))))))

(defmethod acceptor-dispatch-request ((r-actor r-acceptor) request)
  (let ((uri (tbnl:request-uri request))
	(req-type (tbnl:request-method request)))
    (let ((action (router r-actor req-type uri)))
      (if action
	  action
	  (call-next-method)))))

(defmacro compile-routes (&rest routespecs)
  `(list ,@(loop for routespec in routespecs collect
		(apply #'route-compile routespec))))

(defvar *r-host* (make-instance 'r-acceptor :port 4242))
(setf *r-host* (make-instance 'r-acceptor :port 4242))


(setf (dispatch-table *r-host*)
      (compile-routes (:GET "/sr" 'sr)))

(defun sr ()
  (format t "in sr function~%")
  "hello sr")
