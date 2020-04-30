;; Задание 35 - исправил: переписал функцию contain
;;Определите функцию ПОДМНОЖЕСТВО, которая проверяет, 
;;является ли одно множество подмножеством другого. 
;;Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.

(defun contain (list A)
    (cond
        ((null list) nil)
        ((or (eq (car list) A) (contain (cdr list) A)) T)
    )
)

(defun findlist (list sublist)
    (cond
        ((null sublist) T)
        ((and (contain list (car sublist)) (findlist list (cdr sublist))) T)
    )
)

(print "Задание 35")
(print "Case1: (1 2 3 4 5) (7 8 9)")
(print (findlist '(1 2 3 4 5) '(7 8 9)))
(print "Case2: (NIL) (7 8 9)")
(print (findlist '() '(7 8 9)))
(print "Case3: (1 2 3 4 5) (NIL)")
(print (findlist '(1 2 3 4 5) '()))
(print "Case4: (1 2 3 4 5) (3 4 5)")
(print (findlist '(1 2 3 4 5) '(3 4 5)))

;; ------------------------------------
