(in-package :asdk)

(setf *host* (make-instance 'dk-acceptor :port 4242))

(push
 (tbnl:create-prefix-dispatcher "/test" 'test)
 (dispatch-table *host*))
