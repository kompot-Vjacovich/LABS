;; Иванча Николай
;; Вариант 7

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
