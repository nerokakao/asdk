(in-package :asdk)

(defvar *conf-k-v* (make-hash-table :test 'equal))

(setf (gethash "app-root-path" *conf-k-v*) "/Users/nero/daumkakao/playframework/asdk/")
(setf (gethash "pool-1" *conf-k-v*) '("test" "postgres" "asdf1234" "fwq"))
