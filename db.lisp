(in-package :asdk)

(defparameter *pool-1* '("db" "postgres" "postgres" "127.0.0.1")
  "database pool: 1")

(defun test-query-t1-first-name ()
  (car (cdr (car
	     (with-connection *pool-1*
	       (query "select * from t1"))))))

