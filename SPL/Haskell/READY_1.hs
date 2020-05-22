import Data.List

-- Задание 4
-- Определите функцию, разделяющую исходный список на два подсписка.
-- В первый из них должны попасть элементы с нечетными номерами, 
-- а во второй - элементы с четными номерами

evenSplit :: [a] -> [[a]]
evenSplit [] = [[],[]]
evenSplit lst = let (a, as)    = splitAt 1 lst
                    (b, bs)    = splitAt 1 as
                    [as', bs'] = evenSplit bs
                in [a ++ as', b ++ bs']


-- Задание 5
-- Определите функцию, упаковывающую последовательные дубликаты списка
-- в подсписки вида (M N), где N - элемент списка, M - количество повторений.

_group :: (Eq a) => [a] -> [[a]]
_group = foldr (\x acc -> if head acc == [] || head (head acc) == x then (x:head acc) : tail acc else [x] : acc) [[]]

pack lst =  map (\x -> (head x, length x)) (_group lst)


-- Задание 17
-- Определите функцию МНОЖЕСТВО, преобразующую список в множество

quantity = foldl (\seen x ->    if x `elem` seen
                                then seen
                                else seen ++ [x]) []


-- Задание 19
-- Определите функцию ПОДМНОЖЕСТВО, которая проверяет, является ли одно
-- множество подмножеством другого. Определите также СОБСТВЕННОЕ-ПОДМНОЖЕСТВО

subset subs set =
    if null subs
    then True
    else
        if elem (head subs) set
        then subset (tail subs) (delete (head subs) set)
        else False

self_subset subs set = subset set subs && not (null subs) && subset subs set


-- Задание 26
-- Реализовать алгоритм сортировки слиянием.

merge :: (Ord a) => [a] -> [a] -> [a]
merge [] xs = xs
merge xs [] = xs
merge (x:xs) (y:ys)
    | (x < y) = x:merge xs (y:ys)
    | otherwise = y:merge (x:xs) ys
 
mergesort :: (Ord a) => [a] -> [a]
mergesort xs 
    | (length xs) > 1 = merge (mergesort ls) (mergesort rs)
    | otherwise = xs
    where
        n = (length xs) `div` 2
        (ls, rs) = (take n xs, drop n xs)


testing input output case_n = do
    putStr "Case"
    print case_n
    putStr "INPUT: "
    print input
    putStr "OUTPUT: "
    print output

main = do
    print "Task 4"
    testing [1,2,3,4] (evenSplit [1,2,3,4]) 1
    testing [1,2,3] (evenSplit [1,2,3]) 2
    testing [1,2] (evenSplit [1,2]) 3
    print "--------------------------"

    print "Task 5"
    testing ['a','b','b','c','c','d'] (pack ['a','b','b','c','c','d']) 1
    testing ['a','b','c','d'] (pack ['a','b','c','d']) 2
    testing ['a','a','b','c','d','d','d'] (pack ['a','a','b','c','d','d','d']) 3
    print "--------------------------"

    print "Task 17"
    testing ['a','b','b','c','c','d'] (quantity ['a','b','b','c','c','d']) 1
    testing ['a','b','c','d'] (quantity ['a','b','c','d']) 2
    testing ['a','a','b','c','d','d','d'] (quantity ['a','a','b','c','d','d','d']) 3
    print "--------------------------"

    print "Task 19"
    testing (['a','b','c','d'], ['a','z','b','c','y','d']) (subset ['a','b','c','d'] ['a','z','b','c','y','d']) 1
    testing ([1,2,3,4], [5,6,7,8]) (subset [1,2,3,4] [5,6,7,8]) 2
    testing (["null"], [1,2,3,4]) (subset [] [1,2,3,4]) 3
    testing ([1,2], ["null"]) (subset [1,2] []) 4
    print "Self subset"
    testing (['a','b','c','d'], ['a','z','b','c','y','d']) (self_subset ['a','b','c','d'] ['a','z','b','c','y','d']) 1
    testing ([1,2,3,4], [4,3,2,1]) (self_subset [1,2,3,4] [4,3,2,1]) 2
    testing (["null"], [1,2,3,4]) (self_subset [] [1,2,3,4]) 3
    testing ([1,2], ["null"]) (self_subset [1,2] []) 4
    print "--------------------------"

    print "Task 26"
    testing [2,3,1,4] (mergesort [2,3,1,4]) 1
    testing [1,2,1,2,1] (mergesort [1,2,1,2,1]) 2
    testing [1] (mergesort [1]) 3
    testing [1,2,3,4] (mergesort [1,2,3,4]) 4
    print "--------------------------"