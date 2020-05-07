;; Вторая часть

;; Задание 1 
;; Определите FUNCALL через функционал APPLY.

(setq funcall (lambda (&rest x) (apply x)))

(print "Задание 1")
(print "Case1: + 2 3 4 6")
(print(funcall '+ 2 3 4 6))
(print "Case2: - 1")
(print(funcall '- 1))
(print "Case3: * -1 2 -20)")
(print(funcall '* -1 2 -20))
(print "Case4: max -1 -2 -20)")
(print(funcall 'max -1 -2 -20))

;;------------------------------------

;; Задание 3
;; Определите функционал (APL-APPLY f x), который применяет 
;; каждую функцию fi списка (f1 f2 ... fn)
;; к соответствующему элементу списка x = (x1 x2 ... xn)

(defun apl-apply(funcs args)
    (mapcar (lambda (y x) (apply y x)) funcs args)
)

(print "Задание 3")
(print "Case1: (+ -) ((2 3) (4 6))")
(print(apl-apply '(+ -) '((2 3) (4 6))))
(print "Case2: (* / -) ((2 3) (4 6) (1))")
(print(apl-apply '(* / -) '((2 3) (4 6) (1))))
(print "Case3: (max min) ((2 3) (4 6))")
(print(apl-apply '(max min) '((2 3) (4 6))))
(print "Case4: (max) ((2 3) (4 6))")
(print(apl-apply '(max) '((2 3) (4 6))))

;;------------------------------------

;; Задание 5
;; Определите функциональный предикат, 
;; который истинен, когда, являющейся функциональным аргументом
;; предикат истинен хотя бы для одного элемента списка.

(defun any (pred list)
    (cond 
        ((null list) nil)
        ((or (funcall pred (car list)) (any pred (cdr list))))
    )
)

(print "Задание 5")
(print "Case1: atom ((2 3) (4 6))")
(print(any 'atom '((2 3) (4 6))))
(print "Case2: atom ((2 3) (4 6) 1)")
(print(any 'atom '((2 3) (4 6) 1)))
(print "Case3: numberp (a b c d)")
(print(any 'numberp '(a b c d)))
(print "Case4: numberp (a b 2 d)")
(print(any 'numberp '(a b 2 d)))

;;------------------------------------

;; Задание 7
;; Определите фильтр, удаляющий из списка все элементы, 
;; которые не обладают свойством, наличие которого проверяет предикат.

(defun del(pred list) 
    (mapcan (lambda (x) (cond ((funcall pred x) (list x)))) list)
)

(print "Задание 7")
(print "Case1: atom ((2 3) (4 6))")
(print(del 'atom '((2 3) (4 6))))
(print "Case2: atom ((2 3) (4 6) 1)")
(print(del 'atom '((2 3) (4 6) 1)))
(print "Case3: numberp (a b c d)")
(print(del 'numberp '(a b c d)))
(print "Case4: numberp (a b 2 d 4)")
(print(del 'numberp '(a b 2 d 4)))

;;------------------------------------

;; Задание 11
;; Определите фукнционал МНОГОФУН, который использует функции, 
;; являющиеся аргументами, по следующей схеме:
;; (МНОГОФУН ’(f g ... h) x) ⇔ (LIST (f x) (g x) ... (h x)).

(defun multfun(list_of_funcs arg)
    (mapcar (lambda (func) (apply func arg)) list_of_funcs)
)

(print "Задание 11")
(print "Case1: (+ -) (2 3)")
(print(multfun '(+ -) '(2 3)))
(print "Case2: (* / -) (2 3 4 6)")
(print(multfun '(* / -) '(2 3 4 6)))
(print "Case3: (max min) (2 3 4 6)")
(print(multfun '(max min) '(2 3 4 6)))

;;------------------------------------

;; Задание 13
;; Определите функцию, которая возвращает своё определение
(setq retITself
    '(
      (lambda (x) (list x (list 'quote x)))
      '(lambda (x) (list x (list 'quote x)))
     )
)

(print "Задание 13")
(print "Case1:")
(print retITself)

;;------------------------------------

;; Задание 9
;; Генератор чисел Фибоначчи
(defun fib-gen ()
     (let ( (x 0) (y 1) )
         (lambda () 
                 (setq x (+ y (setq y x)))
         )
     )
) 

(defun fib (n f)
    (cond 
        ((= n 0) nil)
        (t
             (cons (funcall f) (fib (- n 1) f))
         )
    )
)

(setq f1 (fib-gen))
(setq f2 (fib-gen))

(print "Задание 9")
(print "Case1: fib 4")
(print (fib '4 f1))
(print "Case2: fib 6")
(print (fib '6 f2))
