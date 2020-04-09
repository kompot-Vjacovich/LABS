;; Иванча Николай
;; Вариант 7

;; Задание 5 - принято
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
(print "Case1: (2 3 4 6)")
(print(list-incr '(2 3 4 6)))
(print "Case2: (NIL)")
(print(list-incr ()))
(print "Case3: (-1 0 -20)")
(print(list-incr '(-1 0 -20)))

;;------------------------------------

;; Задание 9 - исправил: соединил две функции в одну
;; Определите функцию, разделяющую исходный список на два подсписка. В
;; первый из них должны попасть элементы с нечетными номерами, во второй —
;; элементы с четными номерами.

(defun list-split (list)
    (cond
        ((null list) '(nil nil))
        (t
             ((lambda (lst e1 e2)
                 (cons
                    (cons
                        e1 (car lst)
                    )
                    (cond
                        ((null e2) (cdr lst))
                        (t
                            (cons
                                (cons
                                   e2 (cadr lst)
                                )
                                nil
                            )
                        )
                    )
                )
            )
            (list-split(cddr list)) (car list) (cadr list))
        )
    )
)

(print "Задание 9")
(print "Case1: (2 3 4 6)")
(print(list-split '(2 3 4 6)))
(print "Case2: (NIL)")
(print(list-split '()))
(print "Case3: (-1 0 -20)")
(print(list-split '(-1 0 -20)))
(print "Case4: (5)")
(print(list-split '(5)))

;; ------------------------------------

;; Задание 11 - исправил: соединил две функции в одну
;; Определите функцию, осуществляющую разделение исходного списка на два
;; подсписка. В первый из них должно попасть указанное количество элементов
;; с начала списка, во второй — оставшиеся элементы

(defun list-split2 (list n)
    (cond
        ((null list) '(nil nil))
        (t
             ((lambda (lst e1 e2)
                (cons
                    (cons
                        e1 (car lst)
                    )
                    (cond
                        ((null e2) (cdr lst))
                        (t
                            (cons
                                (cons
                                    e2 (cadr lst)
                                )
                                nil
                            )
                        )
                    )
                )
             )
              (list-split(cddr list)) (car list) (cadr list))
        )
    )
)

(print "Задание 11")
(print "Case1: (5 3 1 2 7) 4")
(print(list-split2 '(5 3 1 2 7) 4))
(print "Case2: (2 3 4 6) 1")
(print(list-split2 '(2 3 4 6) 1))
(print "Case3: (NIL) 5")
(print(list-split2 '() 5))
(print "Case4: (1 2 3) 5")
(print(list-split2 '(1 2 3) 5)) 
(print "Case5: (1 2 3) 0")
(print(list-split2 '(1 2 3) 0))

;; ------------------------------------

;; Задание 17
;; Определите функцию, которая осуществляет всевозможные перестановки списка.

(defun ins-in-all-pos (a l r)
  (cond 
      ((null r) (list (append l (list a))))
      (t
          (cons 
               (append l (list a) r) 
               (ins-in-all-pos a (append l (list (car r))) (cdr r))
           )
       )
   )
)
 
(defun all-perms (lst)
  (cond 
      ((null (cdr lst)) (list lst))
      (t 
           (apply 'append (mapcar (lambda (x) (ins-in-all-pos (car lst) nil x)) (all-perms (cdr lst))))
       )
   )
)

(print "Задание 17")
(print "Case1: (1 2 3 4)")
(print(all-perms '(1 2 3 4)))
(print "Case2: (NIL)")
(print(all-perms '()))
(print "Case3: (1)")
(print(all-perms '(1)))

;; ------------------------------------

;; Задание 22 - принято
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
(print "Case1: (1 2 3 4)")
(print(multi-level '(1 2 3 4)))
(print "Case2: (NIL)")
(print(multi-level '()))
(print "Case3: (1)")
(print(multi-level '(1)))

;; ------------------------------------

;; Задание 27 - принято
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
(print "Case1: (1 2) (3 4 5 6)")
(print(mix '(1 2) '(3 4 5 6)))
(print "Case2: (2 3) (a b)")
(print(mix '(2 3) '(a b)))
(print "Case3: (NIL) (a b)")
(print(mix '() '(a b)))
(print "Case4: (2 3) (a b)")
(print(mix '(2 3) ()))
(print "Case5: (2 3) (a)")
(print(mix '(2 3) '(a)))
(print "Case6: (2) (a b)")
(print(mix '(2) '(a b)))
(print "Case7: (2) (c a b)")
(print(mix '(2) '(c a b))) 

;; ------------------------------------

;; Задание 28 - принято
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
(print "Case1: (a (3 (5 6)) 7)")
(print(atom-cnt '(a (3 (5 6)) 7)))
(print "Case2: (2 3 4 6)")
(print(atom-cnt '(2 3 4 6)))
(print "Case3: (NIL)")
(print(atom-cnt '()))
(print "Case4: (a)")
(print(atom-cnt '(a)))
(print "Case5: (a 1)")
(print(atom-cnt '(a 1)))
(print "Case6: (1 a (2 3) b 4)")
(print(atom-cnt '(1 a (2 3) b 4)))
(print "Case7: (a (1 (b (2))))")
(print(atom-cnt '(a (1 (b (2)))))) 

;; ------------------------------------

;; Задание 35 - исправил: переписал функцию contain
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
(print "Case1: (1 2 3 4 5) (7 8 9)")
(print (strstr '(1 2 3 4 5) '(7 8 9)))
(print "Case2: (NIL) (7 8 9)")
(print (strstr '() '(7 8 9)))
(print "Case3: (1 2 3 4 5) (NIL)")
(print (strstr '(1 2 3 4 5) '()))
(print "Case4: (1 2 3 4 5) (3 4 5)")
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

(print "Задание 46")

(set_parents 'ab 'a 'b)
(print "AB is the son of A and B")
(set_parents 'cd 'c 'd)
(print "CD is the son of C and D")
(set_parents 'dc 'c 'd)
(print "DC is the son of C and D")
(set_parents 'ef 'e 'f)
(print "EF is the son of E and F")
(set_parents 'fg 'g 'f)
(print "FG is the son of G and F")

(print "Case1: parents ef?")
(print (parents 'ef))
(print "Case2: brothers ab cd?")
(print (brothers 'ab 'cd))
(print "Case3: brothers dc cd?")
(print (brothers 'dc 'cd))
(print "Case4: brothers ef cd?")
(print (brothers 'ef 'cd))
(print "Case5: brothers ef fg?")
(print (brothers 'ef 'fg))


;; ------------------------------------

;; Задание 48 - принято
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
