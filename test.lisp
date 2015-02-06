(defpackage #:test
  (:use :cl))

(in-package :test)

(defclass acc ()
  ())

(defclass acc-1 (acc)
  ())

(defclass acc-2 (acc)
  ())

(defclass acc-1-1 (acc-1)
  ())

(defmethod fn ((a acc))
  (declare (ignore a))
  (format t "acc~%")
  "r-acc")

(defmethod fn ((a acc-1))
  (declare (ignore a))
  (format t "acc-1~%")
  "r-acc-1"
  (call-next-method))

(defmethod fn ((a acc-2))
  (declare (ignore a))
  (format t "acc-2~%")
  "r-acc-2"
  (call-next-method))

(defmethod fn ((a acc-1-1))
  (declare (ignore a))
  (format t "acc-1-1~%")
  "r-acc-1-1"
  (call-next-method))
