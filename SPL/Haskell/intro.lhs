> import Data.List (foldl')

<h1 id="install">Введение в Haskell</h1>

<h2>Язык программирования Haskell</h2>
<ol>
    <li><b>ghc</b>: компилятор, похожий на gcc для C</li>
    <li><b>ghci</b>: интерактивная оболочка Haskell (REPL)</li>
    <li><b>runhaskell</b>: запуск программы без ее компиляции. Удобно, но очень медленно по сравнению с скомпилированными программами</li>
</ol>

<code class="zsh">
    main = putStrLn "Hello World!"
    Hello World
</code>

<code class="zsh">
    main = do
        print "What's your name?"
        name <- getLine
        print ("Hi " ++ name ++ "!")
</code>

<h2>Определение функций</h2>
<code class="zsh">
    f x y = x + y
</code>

<h2>Пример создания нового типа</h2>

<code class="zsh">
    -- Мы определям тип используя ::
    f :: Int -> Int -> Int
    f x y = x*x + y*y
    main = print (f 2 3)
</code>

> f :: Int -> Int -> Int
> f x y = x*x + y*y

<code class="zsh">
    ~ runhaskell <scriptname>.lhs
    13
</code>

<code class="zsh">
    $ ghci <scriptname>.lhs
    GHCi, version 8.6.3: http://www.haskell.org/ghc/ :? for help
    [1 of 1] Compiling Main ( <scriptname>.lhs, interpreted )
    Ok, one module loaded.
    *Main> :type f
    f :: Num a => a -> a -> a
</code>

На самом деле в Haskell-е ни одна функция не принимает на вход два
аргумента. Вместо этого у всех функций — только один аргумент. Функцию
от двух аргументов можно представить как функцию, принимающую первый
аргумент и возвращающую функцию, принимающую второй аргумент в
качестве параметра.

<code class="zsh">
    > g = f 3
    > g 7
    10
</code>

> g = f 3

Для создания функции можно использовать и лямбда-выражение:
<code class="zsh">
    > g = \y -> y * y
    > g 5
    25
</code>

> g1 = \y -> y * y

<h2>Кортежи</h2>
Задать пару можно следующим образом (a, b). Элементы кортежа могут
иметь различные типы.

<code class="zsh">
    -- Все эти кортежи - валидны
    (2, "foo")
    (3, 'a', [2, 3])
    ((2, "a"), "c", 3)
    fst (x, y) => x
    snd (x, y) => y
    fst (x, y, z) => ERROR: fst :: (a, b) -> a
    snd (x, y, z) => ERROR: snd :: (a, b) -> b
</code>

<h2>Скобки</h2>
Чтобы избавиться от лишних скобок, вы можете воспользоваться этими
функциями: ($) и (.).

<code class="zsh">
    -- По умолчанию:
    f g h x < = > (((f g) h) x)
    -- $ заменяет скобки от $
    -- до конца выражения
    f g $ h x < = > f g (h x) < = > (f g) (h x)
    f $ g h x < = > f (g h x) < = > f ((g h) x)
    f $ g $ h x < = > f (g (h x))
    -- (.) композиция функций
    (f . g) x < = > f (g x)
    (f . g . h) x < = > f (g (h x))
</code>

<h2>Полезные вещи для записи функций</h2>
<code class="zsh">
    x :: Int            ⇔ x is of type Int
    x :: a              ⇔ x can be of any type
    x :: Num a => a     ⇔ x can be any type a
                          such that a belongs to Num type class 
    f :: a -> b         ⇔ f is a function from a to b
    f :: a -> b -> c    ⇔ f is a function from a to (b→c)
    f :: (a -> b) -> c  ⇔ f is a function from (a→b) to c
</code>

Объявлять тип функции перед ее определеним необязательно. Haskell сам
выведет наиболее общий тип. Но указание типа функции — правило хорошего
тона.

<h2>Инфиксная запись</h2>
<code class="zsh">
    square :: Num a => a -> a
    square x = x^2
</code>

> square :: Num a => a -> a
> square x = x^2

Обратите внимание, что ˆ используется в инфиксной нотации. Для каждого
инфиксного оператора существует возможность префиксной записи. Просто
нужно заключить нужный оператор в скобки.

<code class="zsh">
    square' x = (^) x 2
    
    square'' x = (^2) x
</code>

> square' x = (^) x 2
> 
> square'' x = (^2) x

Мы можем убрать x из левой и правой части выражения! Это называется
η-редукция
<code class="zsh">
    square''' = (^2)
</code>

> square''' = (^2)

<h2>Функции высших порядков</h2>
<code class="zsh">
    filter :: (a -> Bool) -> [a] -> [a]
    map :: (a -> b) -> [a] -> [b]
    foldl :: (a -> b -> a) -> a -> [b] -> a
</code>

<code class="zsh">
    evenSum l = mysum 0 (filter even l)
        where
          mysum n [] = n
          mysum n (x:xs) = mysum (n+x) xs
</code>

где

<code class="zsh">
    filter even [1..10] ⇔  [2,4,6,8,10]
</code>

Следующим нашим действием будет упрощение цикла. Для этого воспользуемся функцией foldl, которая позволяет аккумулировать значение:

<code class="zsh">
    myfunc list = foo initialValue list
    foo accumulated []     = accumulated
    foo tmpValue    (x:xs) = foo (bar tmpValue x) xs
</code>

Код выше можно заменить на:

<code class="zsh">
    myfunc list = foldl bar initialValue list
</code>

Код foldl приведен ниже:

<code class="zsh">
    foldl f z [] = z
    foldl f z (x:xs) = foldl f (f z x) xs
</code>

<code class="zsh">
    foldl f z [x1,...xn]  ⇔  f (... (f (f z x1) x2) ...) xn
</code>

<code class="zsh">
    evenSum l = foldl' mysum 0 (filter even l)
        where mysum acc value = acc + value
</code>

Этот код можно упростить, используя лямбда-выражение. Таким образом,
мы избавимся от временного идентификатора mysum.
<code class="zsh">
    evenSum l = foldl' (\x y -> x+y) 0 (filter even l)
</code>

И конечно, можно провести следующую замену

<code class="zsh">
    (\x y -> x+y) ⇔ (+)
</code>

<code class="zsh">
    evenSum :: Integral a => [a] -> a
    evenSum l = foldl (+) 0 (filter even l)
</code>

Другая полезная функция высшего порядка это (.). Функция (.) соответствует
математической композиции.
<code class="zsh">
    (f . g . h) x < = > f ( g (h x))
</code>

Мы можем ее использовать, чтобы еще дальше η-редуцировать нашу функцию:
<code class="zsh">
    import Data.List (foldl')
    evenSum :: Integral a => [a] -> a
    evenSum = (foldl' (+) 0) . (filter even)
</code>

Можно еще так:
<code class="zsh">
    import Data.List (foldl')
    sum' :: (Num a) => [a] -> a
    sum' = foldl' (+) 0
    evenSum :: Integral a => [a] -> a
    evenSum = sum' . (filter even)
</code>


> sum' :: (Num a) => [a] -> a
> sum' = foldl' (+) 0
> evenSum :: Integral a => [a] -> a
> evenSum = sum' . (filter even)

Теперь напишем сумму квадратов

<code class="zsh">
    squareEvenSum = sum' . (filter even) . (map (^2))
    squareEvenSum' = evenSum . (map (^2))
    squareEvenSum'' = sum' . (map (^2)) . (filter even)
    squareEvenSum [1..10]  < = >  220
</code>

> squareEvenSum = sum' . (filter even) . (map (^2))
> squareEvenSum' = evenSum . (map (^2))
> squareEvenSum'' = sum' . (map (^2)) . (filter even)

Функция map просто применяет функцию-параметр ко всем элементам
списка.
<code class="zsh">
    map (^2) [1..4] < = > [1, 4, 9, 16]
</code>

> main = do
>   putStrLn "Hello World!"
>   print "----------------"
>   print "What's your name?"
>   name <- getLine
>   print ("Hi " ++ name ++ "!")
>   print "----------------"
>   print (f 2 3)
>   print "----------------"
>   print (g 7)
>   print "----------------"
>   print (g1 5)
>   print "----------------"
>   print (square 10)
>   print "----------------"
>   print (square' 10)
>   print "----------------"
>   print (square'' 10)
>   print "----------------"
>   print (square''' 10)
>   print "----------------"
>   print (filter even [1..10])
>   print "----------------"
>   print (squareEvenSum [1..10])
>   print "----------------"
>   print (map (^2) [1..4])