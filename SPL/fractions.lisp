(defun split (string &optional (split-character #\Space))
  (let ((result '())
        (stream (make-string-output-stream)))
    (loop for char across string
          if (char= char split-character)
          do (push (get-output-stream-string stream) result)
          else
          do (write-char char stream))
    (push (get-output-stream-string stream) result)
    (nreverse result)))

(defun delSymb (string &optional (symb #\Space))
    (let ((result '())
          (stream (make-string-output-stream)))
         (loop for char across string
               if (not (char= char symb))
               do (write-char char stream))
         (push (get-output-stream-string stream) result)
         (car (nreverse result))))

(defmacro num-tor (f1 f2 sign)
   `(let `(,(n1 (read-from-string (car ,f1)))
           ,(n2 (read-from-string (car ,f2)))
           ,(d1 (read-from-string (cadr ,f1)))
           ,(d2 (read-from-string (cadr ,f2))))
          (funcall sign (* n1 d2) (* n2 d1))))

(defmacro den-tor (f1 f2)
    `(let `(,(d1 (read-from-string (cadr ,f1)))
           ,(d2 (read-from-string (cadr ,f2))))
          (* d1 d2)))

(defun NOD (A B)
    (cond
        ((> A B) (NOD (- A B) B))
        ((< A B) (NOD (- B A) A))
        (B)))



(defun parseToLisp (str)
    (progn 
        (setf elems (split str))
        (setf sign (read-from-string (cadr elems)))
        (setf frac1 (split (car elems) #\/))
        (setf frac2 (split (caddr elems) #\/))
        (setf num (num-tor frac1 frac2 sign))
        (setf denom (den-tor frac1 frac2))
        (setf nod (NOD num denom))
        (setf num (/ num nod))
        (setf denom (/ denom nod))
        (cond 
            ((= num denom) (format t "1"))
            ((= denom 1) (format t "~a" num))
            ((> num denom) (format t "~a ~a/~a" (truncate num denom) (mod num denom) denom))
            (t (format t "~a/~a" num denom)))))

(parseToLisp "4/2 + 2/2")
;(print )
; 3/2 + 2/3
