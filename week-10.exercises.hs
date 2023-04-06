import Prelude hiding (foldl, foldr, sum, product, length, any, 
                       all, minimum, maximum, concat, reverse, filter)

{- 
Foldl, Foldr

sum [] = 0
sum (x:xs) = x + sum xs

product [] = 1
product (x:xs) = x * product xs

Забелязваме ли общ pattern?
-}

foldl :: (b -> a -> b) -> b -> [a] -> b
foldl f acc []     = acc
foldl f acc (x:xs) = foldl f (f acc x) xs

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr f acc []     = acc
foldr f acc (x:xs) = f x (foldr f acc xs)

{-
Задача 1. Използвайте foldr/foldr, за да дефинирате следните функции:
а). sum xs, която връща сбора на елементите на xs.
б). product xs, която връща произведението на елементите на xs.
в). length xs, която връща броя на елементите на xs.
г). any p xs, която връща дали предикатът p e верен за поне един от елементите на xs.
д). all p xs, която връща дали предикатът p e верен за всеки от елементите на xs.
ж). minimum xs, която връща най-малкия елемент на xs.
е). maximum xs, която връща най-големия елемент на xs.

Примери:
    sum [1..10] = 55
    product [1..10] = 3628800
    length [1..10] = 10
    any even [1..10] = True
    all even [1..10] = False
    minimum [1..10] = 1
    maximum [1..10] = 10
-}
sum :: Num a => [a] -> a
sum xs = undefined

product :: Num a => [a] -> a
product xs = undefined

length :: Num b => [a] -> b
length xs = undefined

any :: (a -> Bool) -> [a] -> Bool
any f xs = undefined

all :: (a -> Bool) -> [a] -> Bool
all f xs = undefined

minimum :: Ord a => [a] -> a
minimum xs = undefined

maximum :: Ord a => [a] -> a
maximum xs = undefined


{-
Задача 2. foldl/foldr могат да връщат и списъци: използвайте foldl/foldr, 
за да дефинирате следните функции:
а). concat xss, която приема списък от списъци xss и ги конкатенира в един общ списък.
б). reverse xs, която приема списък xs и обръща елементите му.

Примери: 
    concat [[1], [2], [3]] = [1, 2, 3]
    reverse [1, 2, 3] = [3, 2, 1]
-}
concat :: [[a]] -> [a]
concat xs = undefined

reverse :: [a] -> [a]
reverse xs = undefined
reverse' xs = undefined


{-
Задача 3. Напишете функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))

Този път използвайте fold.

Пример: 
    compose [(+1), (2*)] 7 = (2 * 7) + 1 = 15
-}
compose :: [(a -> a)] -> (a -> a)
compose fs = undefined


{-
Задача 4. Напишете функцията maximize, която получава непразен списък от едноместни
числови функции и връща нова едноместна числова функция на аргумент x, която дава 
стойността f x на тази фунция f от списъка, за която числото f x е най-голямо 
по абсолютна стойност.

Този път използвайте fold.

Пример: 
    maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5  = 1.5
    maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2) = 16
-}
maximize :: [Double -> Double] -> (Double -> Double)
maximize fs = undefined


{-
Задача 4. Напишете функцията filter, използвайки fold.

Пример:
    filter even [1..10] = [2,4,6,8,10]
-}
filter :: (a -> Bool) -> [a] -> [a]
filter f xs = undefined


-- ТЕСТОВЕ НА ЗАДАЧИТЕ --
main = do
    print $ sum      [1..10]
    print $ product  [1..10]
    print $ length   [1..10]
    print $ any even [1..10]
    print $ all even [1..10]
    print $ minimum  [1..10]
    print $ maximum  [1..10]

    print $ concat [[1], [2], [3]]
    print $ reverse [1, 2, 3]

    print $ compose [(+1), (2*)] 7

    print $ maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5
    print $ maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2)

    print $ filter even [1..10]