(defvar *number* "100+300/10")
(defvar *list* '(100 '(PLUS) 200 '(TIMES) 300))
(defvar *nstring* "100+n")


(defun explode (number)
    (coerce number 'list)
)

(defun lexer-helper (lst)
    (cond ( (eq (car lst) nil)
            nil)
          ( (eq (car lst) #\*)
            (cons "TIMES" (lexer (cdr lst))))
          ( (eq (car lst) #\+)
            (cons "PLUS" (lexer (cdr lst))))
          ( (eq (car lst) #\-)
            (cons "MINUS" (lexer (cdr lst))))
          ( (eq (car lst) #\/)
            (cons "DIVIDED BY" (lexer (cdr lst))))
          ( (get-digit 0 lst 1))
    )
)

(defun get-digit (p lst n)
    (if (or (eq lst nil) (not (is-number (car lst))))
        (cons (* p n) (lexer-helper lst))
        (get-digit (- (+ (* p 10) (char-code (car lst))) 48) (cdr lst) n)
    )
    
)

(defun lexer (lst)
    (lexer-helper lst)
)

(defun is-number (ch)
    (if (eq ch nil)
        'f
        (and (>= (char-code ch) 48) (<= (char-code ch) 57))
     )
)

(defun parse-sum-expression (lst)
    (cond ( (eq (car lst) nil)
            nil)
          ( (string= (cadr lst) "PLUS")
            (+ (car lst) (parse-mul-expression (cddr lst))))
          ( (string= (cadr lst) "MINUS")
            (- (car lst) (parse-mul-expression (cddr lst))))
          ( (parse-mul-expression lst))
    )
)

(defun parse-mul-expression (lst)
    (cond ( (eq (car lst) nil)
            nil)
          ( (string= (cadr lst) "TIMES")
            (* (car lst) (parse-root-expression (cddr lst))))
          ( (string= (cadr lst) "DIVIDED BY")
            (/ (car lst) (parse-root-expression (cddr lst))))
          ( (parse-root-expression lst))
    )
)

(defun parse-root-expression (lst)
    (cond ( (eq (cdr lst) nil)
            (car lst))
          ( (parse-expression lst))
    )
)

(defun parse-expression (lst)
    (parse-sum-expression lst)
)

(defun replace-value-in-char-list (lst n)
    (map 'list (lambda (x) (if (eq x #\n) (digit-char n) x)) lst)
)

(print(explode *number*))
(print(lexer(explode *number*)))
(print(parse-expression(lexer(explode *number*))))
(print(replace-value-in-char-list (explode *nstring*) 5))

(defun summationExample1 (x)
    (/ (* (+ x 1) x) 2))

(defun summationExample2 (x)
    (/ (* (* x (+ x 1))(* (+ (* x 2) 1))) 6))

(defun summation (fn i n)
    (if (= i n)
        (funcall fn i)
    (+ (funcall fn i) (summation fn (+ i 1) n) )))

(defun summation-with-string (str i n)
    (if (= i n)
        (parse-expression(lexer(replace-value-in-char-list(explode str) i)))
    (+ (parse-expression(lexer(replace-value-in-char-list(explode str) i))) (summation-with-string str (+ i 1) n) ))
)

(print (summationExample1 2))
(print (summationExample2 2))
(print (summation #'summationExample1 1 5))
(print (summation #'summationExample2 1 5))

(print (summation-with-string "1+4+n" 1 5))