{- Ламбда изрази. Функции от по-висок ред: map & filter -}
import Data.Char -- за примерите по-долу

{-
1. Ламбда изрази (анонимни функции):

 - Имат следната обща форма:
  (\arg1 arg2 ... -> body)

 - Се използват най-често като аргумент или резултат на
   други функции (т.нар. "функции от по-висок ред").

Пример:
    add :: Int -> Int -> Int
    add      x      y  = x + y
    add  =  \x      y -> x + y
    add  =  \x ->  \y -> x + y


2. map f xs връща списък съдържащ резултатите от извикването на f върху
всяко x от оригиналния списък xs.

Примери:
    map (\x -> 2 * x) [1 .. 5]        -> [2, 4, 6, 8, 10]
    map (2 *) [1 .. 5]                -> същото като предишното

    map (\x -> x ^ 2) [1 .. 5]        -> [1, 4, 9, 16, 25]
    map (^ 2) [1 .. 5]                -> същото като предишното


3. filter pred xs връща под-списък съдържащ всички елементи на xs, за които
предикатът pred е верен.

Примери:
    filter (\x -> x `mod` 2 == 0) [1 .. 5]    -> [2, 4]
    filter even [1 .. 5]                      -> същото като предишното

    filter isPrime [1 .. 5]                   -> [2, 3, 5]
-}


{- Примери -}

-- isPrime, sumPrimes и isPerfectNumber от упр. 2
isPrime n = n > 1 && null (filter (\i -> n `mod` i == 0) [2 .. sqrtn]) where
    sqrtn = floor (sqrt (fromIntegral n))

sumPrimes a b = sum (filter isPrime [a .. b])

isPerfectNumber n = n == sum (filter (\i -> n `mod` i == 0) [1 .. n - 1])

-- sumPrimeDivisors от упр. 3
sumPrimeDivisors n = sum (filter isPrime (filter (\i -> n `mod` i == 0) [1 .. n]))

-- isElementOf oт упр. 4
isElementOf x xs = not (null (filter (== x) xs))

-- whisper, shout, switchCaps, removeSpaces от упр. 5
removeSpaces str = filter (not . isSpace) str

whisper str = map toLower str

shout str = map toUpper str

switchCaps str = map (\c -> if isUpper c then toLower c else toUpper c) str


{- Задачи -}
{-
Задача 1. Използвайте рекурсия, за да дефинирате функциите:
а). mapList f xs, същата като map

Пример:
    mapList last ["abc", "def", "ghi"] = "cfi"

б). filterList f xs, същата като filter

Пример:
    filterList isUpper "aBCd" = "BC"
-}
mapList :: (a -> b) -> [a] -> [b]
mapList f xs = undefined

filterList :: (a -> Bool) -> [a] -> [a]
filterList f xs = undefined


{-
Задача 2. Дефинирайте функцията listify xs, която разделя списъка xs
на спицъци, съдържащи всеки от индивидуалните елементи на xs.

Пример:
    listify [1, 2, 3] = [[1], [2], [3]]
-}
listify :: [a] -> [[a]]
listify xs = undefined


{-
Задача 3. Разлика на множества: дефинирайте функцията diff xs ys,
която приема два списъка, във всеки от които няма повтарящи се елементи
и връща разликата между първия и втория, т.е. всички елементи от първия
списък, които не се съдържат във втория.

Пример:
    diff [1, 2, 3] [1, 3, 7] = [2]
-}
diff :: Eq a => [a] -> [a] -> [a]
diff xs ys = undefined


{-
Задача 4. Обединение на множества: дефинирайте функцията union xs ys,
която приема два списъка, във всеки от които няма повтарящи се елементи
и връща обединението им. В резултата също не трябва да има повтарящи се
елементи.

Пример:
    union [1, 2, 3] [1, 3, 7] = [1, 2, 3, 7]
-}
union :: Eq a => [a] -> [a] -> [a]
union xs ys = undefined


{-
Задача 5. Сечение на множества: дефинирайте функцията intersect xs ys,
която приема два списъка, във всеки от които няма повтарящи се елементи
и връща сечението им.

Пример:
    intersect [1, 2, 3] [1, 3, 7] = [1, 3]
-}
intersect :: Eq a => [a] -> [a] -> [a]
intersect xs ys = undefined


{-
Задача 6*. Дефинирайте функцията subsets xs, която връща списък с всички
подсписъци на списъка xs.

Пример:
    subsets [] = [[]]
    subsets [1, 2] = [[1, 2], [1], [2], []]
-}
subsets :: [a] -> [[a]]
subsets xs = undefined


{-
Задача 7**. Дефинирайте функцията pick k xs, която връща списък с всички
възможни избора на k елемента от списъка xs.

Пример:
    pick 2 [1, 2, 3] = [[1, 2], [1, 3], [2, 3]]

В случая примемаме, че изборът [1, 2] е неразличим от [2, 1]

Може да се опитате да решите задачата и в случая, когато ги приемаме
за различни. Тогава:
    pick 2 [1, 2, 3] = [[1, 2], [1, 3], [2, 1], [2, 3], [3, 1], [3, 2]]
-}
pick :: Int -> [a] -> [[a]]
pick k xs = undefined


-- примери от условията
main :: IO ()
main = do
    -- Задача 1.
    print (mapList last ["abc", "def", "ghi"])
    print (filterList isUpper "aBCd")

    -- Задача 2.
    print (listify [1, 2, 3])

    -- Задача 3.
    print (diff [1, 2, 3] [1, 3, 7])

    -- Задача 4.
    print (union [1, 2, 3] [1, 3, 7])

    -- Задача 5.
    print (intersect [1, 2, 3] [1, 3, 7])

    -- Задача 6.
    print (subsets ([]::[Int]))
    print (subsets [1, 2])

    -- Задача 7.
    print (pick 2 [1, 2, 3])
