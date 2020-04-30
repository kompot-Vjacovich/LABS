;; Задание 35 - исправил: переписал функцию subset
;;Определите функцию ПОДМНОЖЕСТВО, которая проверяет, 
;;является ли одно множество подмножеством другого. 
;;Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.

(defun contain (list A)
    (cond
        ((null list) nil)
        ((or (eq (car list) A) (contain (cdr list) A)) T)
    )
)

(defun subset (list sublist)
    (cond
        ((null sublist) T)
        ((and (contain list (car sublist)) (subset list (cdr sublist))) T)
    )
)

(defun self-subset (list sublist)
    (cond
        ((null sublist) nil)
        ((and (subset list sublist) (subset sublist list)) nil)
        (T (subset list sublist))
    )
)

(print "Задание 35")
(print "Case1: (1 2 3 4 5) (7 8 9)")
(print (subset '(1 2 3 4 5) '(7 8 9)))
(print "Case2: (NIL) (7 8 9)")
(print (subset '() '(7 8 9)))
(print "Case3: (1 2 3 4 5) (NIL)")
(print (subset '(1 2 3 4 5) '()))
(print "Case4: (1 2 3 4 5) (3 4 5)")
(print (subset '(1 2 3 4 5) '(3 4 5)))
(print "Case5: (1 2 3 4 5) (1 2 3 4 5)")
(print (subset '(1 2 3 4 5) '(1 2 3 4 5)))
(print "Self-subset cases")
(print "Case1: (7 8 9) (7 8 9)")
(print (self-subset '(7 8 9) '(7 8 9)))
(print "Case2: (1 2 3 4 5) (NIL)")
(print (self-subset '(1 2 3 4 5) '()))
(print "Case3: (1 2 3 4 5) (3 4 5)")
(print (self-subset '(1 2 3 4 5) '(3 4 5)))

;; ------------------------------------
