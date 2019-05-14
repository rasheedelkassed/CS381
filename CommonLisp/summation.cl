(defun summationExample1 (x)
    (/ (* (+ x 1) x) 2))

(defun summationExample2 (x)
    (/ (* (* x (+ x 1))(* (+ (* x 2) 1))) 6))

(defun summation (fn i n)
    (if (= i n)
        (funcall fn i)
    (+ (funcall fn i) (summation fn (+ i 1) n) )))

(print (summationExample1 2))
(print (summationExample2 2))
(print (summation #'summationExample1 1 5))
(print (summation #'summationExample2 1 5))