(defvar *test-name* nil)

(defmacro deftest (name parameters &body body)
    "Функция определённая с помощью deftest может быть использованна
    для вызова других 'тестовых' функций или может быть протестированна
    с помощью check."
    `(defun ,name ,parameters
    	(let ((*test-name* (append *test-name* (list ',name)))) ,@body)
    )
)

(defmacro check (&body forms)
	"Тестирует каждое выражение из forms"
	`(combine
		,@(loop for f in forms collect `(testing-result ,f ',f))
	)
)

(defmacro combine (&body forms)
    "Проверяет наличие хотя бы одного неправильного результата
    среди результатов всех тестов из forms"
    (with-gensym (result)
        `(let ((,result t))
            ,@(loop for f in forms collect `(unless ,f (setf ,result nil)))
            ,result
        )
    )
)

(defun testing-result (result form)
    "Тестирование выражения"
    (format t "~:[FAIL~;SUCCESS~] ... ~a: ~a~%" result *test-name* form)
    result
)

(defmacro with-gensym ((&rest names) &body body)
    "Связывает каждую переменную из names с сгенерированным символом"
	`(let ,(loop for n in names collect `(,n (gensym))) ,@body)
)

(defun run(multitest)
    (format t "... ~:[TESTING FAILED~;TESTING PASSED SUCCESSFULY~] ..." (funcall multitest))
)

(defun l= (lst1 lst2) (and (every '= lst1 lst2) (= (length lst1) (length lst2))))

;; ТЕСТЫ
; Математические функции
; Сложение
(defun test_+ ()
    (check
        (= (+ 1 2) 3)
        (= (+ 1 2 3) 6)
        (= (+ -1 -3) -4)))
; Умножение
(defun test_* ()
    (check
        (= (* 2 2) 4)
        (= (* 3 5) 15)
        (= (* -2 3) -6)))
(deftest test_math ()
    (combine
        (test_+)
        (test_*)))
;--------------------
; Функции работы со списками
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
(deftest test_mix ()
    (check
        (l= (mix '(1 2) '(3 4 5 6)) '(1 3 2 4 5 6))))

(deftest test_all ()
    (combine
         (test_math)
         (test_mix)))

(run 'test_all)
;(print (macroexpand '(combine (1 2 3 4))))
