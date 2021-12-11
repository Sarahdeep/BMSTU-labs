(load "tests.rkt")
(define the-tests
  (list (test (derivative -2) -1)
        (test (signum  0)  0)
        (test (signum  2)  1)))


(run-tests the-tests)

(run-test (test (signum  0)  0))