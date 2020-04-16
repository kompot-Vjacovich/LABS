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

;; ------------------------------------

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
