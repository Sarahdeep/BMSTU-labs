(define-syntax for
  (syntax-rules ( in as)
    ((for x in '(num) expr ...)
     ((lambda (x) (begin expr ...)) num))
    ((for x in '(num . xs) expr ...)
     (begin
       ((lambda (x) (begin expr ...)) num)
       (for x in 'xs expr ...)))

    ((for '(num) as x expr ...)
     ((lambda (x) (begin expr ...)) num))
    ((for '(num . xs) as x expr ...)
     (begin
       ((lambda (x) (begin expr ...)) num)
       (for 'xs as x expr ...)))))

(for i in '(1 2 3)
  (for j in '(4 5 6)
    (display (list i j))
    (newline)))

(for '(1 2 3) as i
  (for '(4 5 6) as j
    (display (list i j))
    (newline)))

(define-syntax while
  (syntax-rules ()
    ((while cond? expr ...)
     (let loop ((xd cond?))
       (if xd
           (begin expr ...
                  (loop cond?)))))))
     
(let ((p 0)
      (q 0))
  (while (< p 3)
         (set! q 0)
         (while (< q 3)
                (display (list p q))
                (newline)
                (set! q (+ q 1)))
         (set! p (+ p 1))))


(define-syntax repeat
  (syntax-rules (until)
    ((repeat (expr ...) until cond?)
     (let loop ((xd cond?))
       (begin
         expr ...
         (if (not xd)
             (loop cond?)))))))
(define-syntax cout
  (syntax-rules (<< endl)
    ((cout << endl)
     (display #\newline))
    ((cout << endl . others)
     (begin
       (display #\newline)
       (cout . others)))
    ((cout << expr . others)
     (begin
       (display expr)
       (cout . others)))
    ((cout << expr)
     (display others))))
     
