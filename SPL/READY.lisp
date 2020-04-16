;; Задание 11 - исправил: соединил две функции в одну
;; Определите функцию, осуществляющую разделение исходного списка на два
;; подсписка. В первый из них должно попасть указанное количество элементов
;; с начала списка (n), во второй — оставшиеся элементы

(defun list-split2 (list n)
    (cond
        ((null list) '(nil nil))
        (t
             ((lambda (lst e1 n)
                (cond
                    ((> n 0)
                         (cons
                             (cons
                                  e1 (car lst)
                             )
                             (cdr lst)
                         )
                    )
                    ((<= n 0)
                         (cons
                             (car lst)
                             (cons
                                  (cons 
                                      e1 (cadr lst)
                                  )
                                  nil
                             )
                        )
                    )
                )
             )
              (list-split2 (cdr list) (- n 1)) (car list) n)
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

;; Задание 17 - исправил: без функционалов
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
 
(defun add-elem-for-each-permutation (elem perm-lst)
	(cond
		((null perm-lst) nil)
		(t (append
				(ins-in-all-pos elem nil (car perm-lst))
				(add-elem-for-each-permutation elem (cdr perm-lst))))
	)
)

(defun all-perms (lst)
	(cond
		((null lst) nil)
		((null (cdr lst)) (list lst))
		(t (add-elem-for-each-permutation
			(car lst)
			(all-perms (cdr lst))))
	)
)

(print "Задание 17")
(print "Case1: (1 2 3)")
(print(all-perms '(1 2 3)))
(print "Case2: (NIL)")
(print(all-perms '()))
(print "Case3: (1)")
(print(all-perms '(1)))

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
