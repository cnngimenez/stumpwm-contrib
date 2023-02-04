;;;; package.lisp

(defpackage #:cpu
  (:use #:cl :stumpwm)
  (:export #:set-cpu-number
           #:*cpu-modeline-fmt*
           #:*acpi-thermal-zone*))

