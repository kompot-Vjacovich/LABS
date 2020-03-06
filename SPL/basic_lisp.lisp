;; Иванча Николай
;; Вариант 7

;; Задание 5
;; Определите функцию, которая увеличивает элементы исходного списка на единицу
(defun list-incr (list)
    (cond
        ((null list) nil)
        (t 
            (cons 
                (+ (car list) 1) 
                (list-incr (cdr list))
            )
        )
    )
)
(print(list-incr '(2 3 4 6)))

;; (2 3 4 6) -> (3 4 5 7)
;; () -> NIL
;; (-1 0 -20) -> (0 1 -19)

------------------------------------

;; Задание 9
;; Определите функцию, разделяющую исходный список на два подсписка. В
;; первый из них должны попасть элементы с нечетными номерами, во второй —
;; элементы с четными номерами.

(defun list-split (list)
    (cond
        ((null list) '(nil nil))
        (t
            (nemerge (list-split(cddr list)) (car list) (cadr list))
        )
    )
)

(defun nemerge(list e1 e2)
    (cons
        (cons
            e1 (car list)
        )
        (cond
            ((null e2) (cdr list))
            (t
                (cons
                    (cons
                        e2 (cadr list)
                    )
                    nil
                )
            )
        )
    )
)

(print(list-split '(2 3 4 6)))

;; (2 3 4 6) -> ((2 4) (3 6))
;; () -> (NIL NIL)
;; (-1 0 -20) -> ((-1 -20) (0)) 
;; (5) -> ((5) NIL)

;; ------------------------------------

;; Задание 11
;; Определите функцию, осуществляющую разделение исходного списка на два
;; подсписка. В первый из них должно попасть указанное количество элементов
;; с начала списка, во второй — оставшиеся элементы

(defun list-split2 (list n)
    (cond
        ((null list) '(nil nil))
        (t
            (nemerge2 
                 (list-split2 (cdr list) (- n 1)) 
                 (car list) 
                 n
             )
        )
    )
)

(defun nemerge2(list e1 n)
    (cond
        ((> n 0)
             (cons
                 (cons
                      e1 (car list)
                 )
                 (cdr list)
             )
        )
        ((<= n 0)
            (cons
                 (car list)
                 (cons
                      (cons 
                          e1 (cadr list)
                      )
                      nil
                 )
            )
        )
    ) 
)

(print(list-split2 '(5 3 1 2 7) 4))

;; (2 3 4 6) 1 -> ((2) (3 4 6))
;; () 5 -> (NIL NIL)
;; (1 2 3) 5 -> ((1 2 3) NIL) 
;; (1 2 3) 0 -> (NIL (1 2 3))

;; ------------------------------------

;; Задание 22
;; Определите функцию, которая обращает список (а b с) и разбивает его на
;; уровни (((с) b) а).

(defun multi-level (list)
    (cond
        ((null list) '(nil))
        ((null (cdr list)) (cons (car list) nil))
        (t
             (cons 
                  (multi-level (cdr list))
                  (cons
                       (car list)
                       nil
                  )
             )
        )
    )
)

(print(multi-level '(1 2 3 4)))

;; (2 3 4 6) -> ((((6) 4) 3) 2)
;; () -> (NIL)
;; (1) -> (1) 

;; ------------------------------------

;; Задание 27
;; Определите функцию, которая, чередуя элементы списков (a b...) и (1 2...),
;; образует новый список (a 1 b 2 ...).

(defun mix (list1 list2)
    (cond
        ((null list1) list2)
        ((null list2) list1)
        (t
             (cons 
                  (car list1)
                  (cons 
                       (car list2)
                       (mix (cdr list1) (cdr list2))
                  )
             )
        )
    )
)

(print(mix '(1 2) '(3 4 5 6)))

;; (2 3) (a b) -> (2 a 3 b)
;; () (a b) -> (a b)
;; (2 3) () -> (2 3)
;; (2 3) (a) -> (2 a 3)
;; (2) (a b) -> (2 a b)
;; (2) (c a b) -> (2 c a b) 

;; ------------------------------------

;; Задание 28
;; Определите функцию, вычисляющую, сколько всего атомов в списке (списочной структуре)

(defun atom-cnt (list &optional (cnt 0))
    (cond
        ((null list) cnt)
        ((atom (car list))
             (atom-cnt (cdr list) (+ cnt 1))
        ) 
        (t 
             (atom-cnt 
                  (cdr list)
                  (atom-cnt (car list) cnt)
             )
        )
    )
)

(print(atom-cnt '(a (3 (5 6)) 7)))

;; (2 3 4 6) -> 4
;; () -> 0
;; (a) -> 1
;; (a 1) -> 2
;; (1 a (2 3) b 4) -> 6
;; (a (1 (b (2)))) -> 4 

;; ------------------------------------