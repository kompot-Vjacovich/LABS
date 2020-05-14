;; Задание 1
;; Определите макрос, который возвращает свой вызов

(defmacro self (&rest arg)
    `(quote (self ,arg))
)

(print "Задание 1")
(print "Case1: (self 2 3 4 6)")
(print(self 2 3 4 6))
(print "Case2: (self 1)")
(print(self 1))
(print "Case3: (self)")
(print(self))

;; ------------------------------------

;; Задание 3
;; Определите лисповскую форму (IF условие p q) в виде макроса

(defmacro new-if (condition p q)
    `(cond 
         (,condition ,p)
         (t ,q)
     )
)

(print "Задание 3")
(print "Case1: (new-if (< 2 5) 1 0)")
(print(new-if (< 2 5) 1 0))
(print "Case2: (new-if (< 5 2) 1 0)")
(print(new-if (< 5 2) 1 0))

;; ------------------------------------

;; Задание 4
;; Определите в виде макроса форму (FIF тест отр нуль полож).

(defmacro fif (test neg nul pos)
    `(cond 
         ((< ,test 0) ,neg)
         ((= ,test 0) ,nul)
         (t  ,pos)
      )
)

(print "Задание 4")
(print "Case1: (fif -5 -1 0 1)")
(print(fif -5 -1 0 1))
(print "Case2: (fif 0 -1 0 1)")
(print(fif 0 -1 0 1))
(print "Case3: (fif 5 -1 0 1)")
(print(fif 5 -1 0 1))

;; ------------------------------------

;; Задание 2
;; Определите макрос (POP стек), который читает из стека верхний элемент и
;; меняет значение переменной стека.

(defmacro pop1 (stack)
    `(prog1 (car ,stack)
        (setq ,stack (cdr ,stack))
    )
)
(setq stack '(1 2 3 4))
(print "Задание 2: ")
(print stack)
(print "Case1:")
(print(pop1 stack))
(print stack)
(print "Case2:")
(print(pop1 stack))
(print stack)
(print "Case3:")
(print(pop1 stack))
(print stack)
