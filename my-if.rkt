(define-syntax my-if
  (syntax-rules ()
  ((my-if cond t f)
    (let ((ti (delay t))
      (fi (delay f)))
      (force (or (and cond  ti)  fi))))))
 
  