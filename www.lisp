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

