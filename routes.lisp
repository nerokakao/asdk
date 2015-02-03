(in-package :asdk)

;(setf *host* (make-instance 'dk-acceptor :port 4242))

(push
 (tbnl:create-prefix-dispatcher "/test" #'test)
 (dispatch-table *host*))

(push (tbnl:create-static-file-dispatcher-and-handler
       "/index"
       #p"/Users/nero/daumkakao/playframework/asdk/www/index.html")
      (dispatch-table *host*))


(defun url/index (static-file)
  (push (tbnl:create-static-file-dispatcher-and-handler "/index"
							static-file)
	(dispatch-table *host*)))

