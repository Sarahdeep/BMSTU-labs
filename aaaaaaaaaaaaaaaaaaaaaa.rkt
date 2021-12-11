(load "trace.scm")
(define counter
  (let ((x 0))
    (lambda () (set! x (+ 1 x)) x)))