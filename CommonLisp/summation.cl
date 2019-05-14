(defun summationExample1 (x)
    (/ (* (+ x 1) x) 2))

(defun summationExample2 (x)
    (/ (* (* x (+ x 1))(* (+ (* x 2) 1))) 6))

(print (summationExample1 2))
(print (summationExample2 2))