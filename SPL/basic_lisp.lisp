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
(print "Задание 5")
(print(list-incr '(2 3 4 6)))
(print(list-incr ()))
(print(list-incr '(-1 0 -20)))

;;------------------------------------

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

(print "Задание 9")
(print(list-split '(2 3 4 6)))
(print(list-split '()))
(print(list-split '(-1 0 -20)))
(print(list-split '(5)))

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

(print "Задание 11")
(print(list-split2 '(5 3 1 2 7) 4))
(print(list-split2 '(2 3 4 6) 1))
(print(list-split2 '() 5))
(print(list-split2 '(1 2 3) 5)) 
(print(list-split2 '(1 2 3) 0))

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

(print "Задание 22")
(print(multi-level '(1 2 3 4)))
(print(multi-level '()))
(print(multi-level '(1)))

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

(print "Задание 27")
(print(mix '(1 2) '(3 4 5 6)))
(print(mix '(2 3) '(a b)))
(print(mix '() '(a b)))
(print(mix '(2 3) ()))
(print(mix '(2 3) '(a)))
(print(mix '(2) '(a b)))
(print(mix '(2) '(c a b))) 

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

(print "Задание 28")
(print(atom-cnt '(a (3 (5 6)) 7)))
(print(atom-cnt '(2 3 4 6)))
(print(atom-cnt '()))
(print(atom-cnt '(a)))
(print(atom-cnt '(a 1)))
(print(atom-cnt '(1 a (2 3) b 4)))
(print(atom-cnt '(a (1 (b (2)))))) 

;; ------------------------------------

;; Задание 35
;;Определите функцию ПОДМНОЖЕСТВО, которая проверяет, 
;;является ли одно множество подмножеством другого. 
;;Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО.

(defun contain (str A)
    (cond
        ((null str) nil)
        ((or (eq (car str) A) (contain (cdr str) A)) T)
    )
)

(defun strstr (str substr)
    (cond
        ((null substr) T)
        ((and (contain str (car substr)) (strstr str (cdr substr))) T)
    )
)

(print "Задание 35")
(print (strstr '(1 2 3 4 5) '(7 8 9)))
(print (strstr '() '(7 8 9)))
(print (strstr '(1 2 3 4 5) '()))
(print (strstr '(1 2 3 4 5) '(3 4 5)))

;; ------------------------------------

;; Задание 46
;;Предположим, что отец и мать некоторого лица, 
;;хранятся как значения соответствующих свойств у символа, обозначающего это лицо. 
;;Напишите функцию (РОДИТЕЛИ x), которая возвращает в качестве значения родителей, 
;;и предикат (СЕСТРЫ-БРАТЬЯ x1 x2), который истинен в случае, 
;;если x1 и x2 — сестры или братья, родные или с одним общим родителем.

(defun parents(x)
    (list (get x 'mom) (get x 'dad))
)

(defun set_parents(x dad mom)
    (setf (get x 'mom) mom)
    (setf (get x 'dad) dad)
)

(defun brothers(x y)
    (cond
        ((equal (get x 'dad) (get y 'dad)) T)
        ((equal (get x 'mom) (get y 'mom)) T)      
        (T nil)
    )
)

(set_parents 'ab 'a 'b)
(set_parents 'cd 'c 'd)
(set_parents 'dc 'c 'd)
(set_parents 'ef 'e 'f)
(set_parents 'fg 'g 'f)

(print "Задание 46")
(print (parents 'ef))
(print (brothers 'ab 'cd))
(print (brothers 'dc 'cd))
(print (brothers 'ef 'cd))
(print (brothers 'ef 'fg))


;; ------------------------------------

;; Задание 48
;Функция GET возвращает в качестве результата NIL в том случае, 
;;если у символа нет данного свойства, либо если значением этого свойства является NIL.
;;Следовательно, функцией GET нельзя проверить, есть ли некоторое свойство в списке свойств. 
;;Напишите предикат (ИМЕЕТ-СВОЙСТВО символ свойство), который проверяет, обладает ли символ данным свойством.

(defun HasProp(x prop)
  (FindProp prop (symbol-plist x))
)


(defun FindProp(prop list)
  (cond
      ((null list) nil)
      ((equal prop (car list)) T)
      (T (FindProp prop (cddr list)))
  )
)

(setf (get 'Smth 'prop1) 1)
(setf (get 'Smth 'prop2) 2)
(setf (get 'Smth 'prop3) 'three)

(print "Задание 48")
(print (HasProp 'Smth 'prop1))
(print (HasProp 'Smth 'prop2))
(print (HasProp 'Smth 'prop3))
(print (HasProp 'Smth 'prop4))
(print (HasProp 'Smth2 'prop4))
