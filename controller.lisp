(in-package :asdk)

(defun routes ()
  (tbnl:request-uri*))

(defun login ()
  (tbnl:get-parameter "a"))

