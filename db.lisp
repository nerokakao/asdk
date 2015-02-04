(in-package :asdk)

(defun test-query-t1-first-name ()
  (car (cdr (car
	     (with-connection (gethash "pool-1" *conf-k-v*)
	       (query "select * from employees"))))))

