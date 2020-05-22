;; Задание 5 - подправил
;; Определите функциональный предикат, 
;; который истинен, когда, являющейся функциональным аргументом
;; предикат истинен хотя бы для одного элемента списка.

(defun any (pred lst)
    (not
        (null (mapcan (lambda (x) (cond ((funcall pred x) (list x)))) lst))
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

;; Задание 13 - эту вы пропустили в прошлый раз
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
