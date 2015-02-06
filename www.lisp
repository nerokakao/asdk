(in-package :asdk)

(defvar *host* nil
  "webapp host")

(defvar *dispatch-tables* '())

(defclass dk-acceptor (tbnl:acceptor)
  ()
  (:documentation
   "This is the acceptor of the ``daumkakao``
   hunchentoot framework"))

(defmethod acceptor-dispatch-request ((acceptor dk-acceptor) request)
  "The easy request dispatcher which selects a request handler
   based on a list of individual request dispatchers all of which can
   either return a handler or neglect by returning NIL."
  (loop for dispatcher in *dispatch-tables*
     for action = (funcall dispatcher request)
     when action return (funcall action)
     finally (call-next-method)))

(defun dk-start (port)
  (if (eql *host* nil)
      (setf *host* (make-instance 'dk-acceptor :port port)))
  (tbnl:start *host*))

(defun dk-stop ()
  (if (not (eql *host* nil))
      (tbnl:stop *host*)))

