{- Рекурсивни функции върху числа -}

{-
Списъците (Lists) (заедно с векторите (кортежи, Tuples), който ще разгледаме
по-нататък) са един от основните съставни типове данни в Хаскел.

Списъците могат да съдържат елементи от произволен тип, но всички елементи на
даден списък ТРЯБВА да бъдат от един и същи тип. Това определя и типа на самия
списък, напр. [Int] e списък от цели числа (Int), докато [[Double]] е списък
от списъци от числа с плаваща запетая (Double).

По конвенция, в Хаскел имената на променливите, които са списъци винаги завършват
с буквата 's' (както думите в множествено число в английскя език).


Ето и някои основни операции върху списъци:

1. Създаване:

- директно изброяване на елементите:
  xs = [1, 2, 3, 4, 5]
  [] - създава празен списък

- дефиниране на интервал (range):
  [1 .. 10]                 -> [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

- интервал със стъпка (равна на разликата м/у първия и нулевия елементи):
  [1, 3 .. 10]              -> [1, 3, 5, 7, 9]


2. Добавяне на елементи:
- добавяне на елемент в началото:
  123 : xs                  -> [123, 1, 2, 3, 4, 5]

  Операторът (:) се нарича cons, съкратено от construct.

- слепване (конкатенация) на два или повече списъка:
  [1, 3, 5] ++ [2, 4, 6]    -> [1, 3, 5, 2, 4, 6]
  [11] ++ [22] ++ [33]      -> [11, 22, 33]

- В Хаскел няма оператор за добавяне в края на даден списък, но за целта
  може да използвате следния "трик":
  xs ++ [345]               -> [1, 2, 3, 4, 5, 345]


2. Полезни функции:

- null xs връща дали списъкът е празен:
  null [1, 2, 3, 4, 5]      -> False
  null []                   -> True

- length xs връща дължината на списъка:
  length [1, 2, 3, 4, 5]    -> 5
  length []                 -> 0

- head xs връща първия елемент на списъка (или грешка ако е празен!):
  head [1, 2, 3, 4, 5]      -> 1
  head []                   -> Exception: Prelude.head: empty list

- tail xs връща всички БЕЗ първия елемент:
  tail [1, 2, 3, 4, 5]      -> [2, 3, 4, 5]
  tail []                   -> []

- take n xs връща първите n елемента на xs:
  take 2 [1, 2, 3, 4, 5]    -> [1, 2]
  take 10 [1, 2, 3, 4, 5]   -> [1, 2, 3, 4, 5] - ако n >= length xs, take връща
                                                 целия списък

- drop n xs връща всички БЕЗ първите n елемента от xs:
  drop 2 [1, 2, 3, 4, 5]    -> [3, 4, 5]
  drop 10 [1, 2, 3, 4, 5]   -> [] - ако n >= length xs, drop връща празен списък

- reverse xs връща списък с елементите в обратен ред:
  reverse [1, 2, 3, 4, 5]   -> [5, 4, 3, 2, 1]
  reverse []                -> []

- elem x xs ни казва дали x е елемент на списъка xs:
  2 `elem` [1, 2, 3, 4, 5]  -> True
  10 `elem` [1, 2, 3, 4, 5] -> False

- minimum, maximum, sum, product - правят, каквото очаквате:

  minimum [1, 2, 3, 4, 5]   -> 1
  minimum []                -> Exception: Prelude.minimum: empty list

  maximum [1, 2, 3, 4, 5]   -> 5
  maximum []                -> Exception: Prelude.maximum: empty list

  sum [1, 2, 3, 4, 5]       -> 15 = 1 + 2 + 3 + 4 + 5
  sum []                    -> 0

  product [1, 2, 3, 4, 5]   -> 120 = 1 * 2 * 3 * 4 * 5
  product []                -> 1
-}


{- Примери -}
-- Пример 1. Функцияtта sum xs, връщаща сбора на елементите в списъса xs.
-- Вариант 1. Стандартна рекурсия
sum' :: [Integer] -> Integer
sum' xs =
    if null xs                          -- проверка, дали списъкът е празен
        then 0                          -- дъно на рекурсията
        else head xs + sum (tail xs)    -- head връща първия елемент,
                                        -- а tail всички останали елементи на списъка

-- Вариант 2. Същата логика, но с опашкова рекурсия
sum'' :: [Integer] -> Integer
sum'' xs = helper xs 0 where
    helper items res
        | null items = res
        | otherwise  = helper (tail items) (res + head items)


-- Пример 2. Функцията reverse xs, която обръща реда на елементите на списъка xs.
reverse' :: [t] -> [t]
reverse' xs = helper xs [] where
    helper items res =
        if null items
            then res
            else helper (tail items) (head items:res)

reverse'' :: [t] -> [t]
reverse'' xs
    | null xs   = []
    | otherwise = reverse (tail xs) ++ [head xs]    -- xs ++ [x], е прост hack, за да добавим
                                                    -- елемента x накрая на списъка xs.

-- Пример 3. Функцията take n xs, която връща първите n елемента на списъка xs.
take' :: Int -> [t] -> [t]
take' n xs
    | n <= 0    = []
    | null xs   = []
    | otherwise = head xs : take (n-1) (tail xs)

-- В повечето случай е по-добре да добавяме в началото на списък и наркая да обърнем резултата,
-- отколкото да използваме ++ [x] hack-а, за да добавяме в края на списъка.
take'' :: Int -> [t] -> [t]
take'' n xs = reverse (helper n xs []) where
    helper i items res
        | i <= 0        = res
        | null items    = res
        | otherwise     = helper (i-1) (tail items) (head items:res)


{-
P.S. Използването на списъци и функции върху тях може сериозно да опрости решенито
на някои задачи, например:
-}
factorial :: Integer -> Integer
factorial n = product [1 .. n]

sumNumbers :: Integer -> Integer -> Integer
sumNumbers a b = sum [a .. b]


{- Задачи -}
{-
Задача 1. Да се дефинира функцията listLength xs, която приема списък xs и
връща неговата дължина (подобно на функцията length от стандартната Прелюдия).

Примери:
    listLength [] = 0
    listLength [1, 2, 3, 4] = 4
-}
listLength :: [a] -> Int
listLength xs =
    if null xs
        then 0
        else 1 + listLength (tail xs)


{-
Задача 2. Да се дефинира функцията isElementOf x xs, която приема дадено число x
и списък от числа xs и връща дали x се съдържа в xs (подобно на функцията elem).

Примери:
    3 `isElementOf` [1, 2, 3, 4] = True
    5 `isElementOf` [1, 2, 3, 4] = False
-}
isElementOf :: Integer -> [Integer] -> Bool
isElementOf x xs
    | null xs       = False
    | x == head xs  = True
    | otherwise     = isElementOf x (tail xs)


{-
Задача 3. Да се дефинира функция interval a b, която връща списък с числата в
интервала [a .. b] (за целта НЕ може да използвате израза [a .. b]).

Примери:
    interval 1 4 = [1, 2, 3, 4]
    interval 2 5 = [2, 3, 4, 5]
    interval 5 2 = []
-}
interval :: Integer -> Integer -> [Integer]
interval a b =
    if a > b
        then []
        else a : interval (a+1) b


{-
Задача 4. Да се дефинира функция digits n, която връща списък с цифрите на
цялото число n >= 0.

Примери:
    digits 1234 = [1, 2, 3, 4]
    digits 1750 = [1, 7, 5, 0]
-}
digits :: Integer -> [Integer]
digits n = reverse (helper n) where
    helper n
        | n < 0     = error "n < 0"
        | n < 10    = [n]
        | otherwise = (n `mod` 10) : helper (n `div` 10)

digits' :: Integer -> [Integer]
digits' n
    | n < 0  = error "n < 0"
    | n < 10 = [n]
    | otherwise = digits' (n `div` 10) ++ [n `mod` 10]


{-
Задача 5. Да се дефинира функция removeDuplicates xs, която приема списък от
числа xs, и връща списък от числа, в който са премахнати всички дупликати в xs.

Примери:
    removeDuplicates [1, 2, 1] = [1, 2]
    removeDuplicates [1, 3, 7, 3, 5, 1] = [1, 3, 7, 5]
-}
removeDuplicates :: [Integer] -> [Integer]
removeDuplicates xs = reverse (helper xs []) where
    helper items res
        | null items                    = res
        | isElementOf (head items) res  = helper (tail items) res
        | otherwise                     = helper (tail items) (head items:res)

-- Решение без помощна ф-я, но което запазва последаната (а не първата)
-- инстанция на всеки елемент от списъка
removeDuplicates' :: [Integer] -> [Integer]
removeDuplicates' xs
    | null xs                = []
    | head xs `elem` tail xs = removeDuplicates' (tail xs)
    | otherwise              = head xs : removeDuplicates' (tail xs)


{-
Задача 6*. Mergesort: mergesort e един от най-ефикасните алгоритми за сортиране, особено що се отнася до
функционални езици. Функцията sort от модула Data.List, която е стандартния начин за сортиране на списъци
в Haskell, използва - макар и малко по-оптимизиран от този, който ще имплементирате - вариант на този алгоритъм.

а). Нaпишете функцията merge xs ys, която приема два списъка подредени в нарастващ ред и ги
обединява в един списък, чийто елементи също са подредени в нарастващ ред.

Пример:
    merge [1, 3, 7] [2, 4, 6] = [1, 2, 3, 4, 6, 7]

б). Използвайте функцията от предишната подточка и идеята, че мога да сортирам списък като го
разделя на две половини, сортирам всяка от тях поотделно и после ги обединя - което е пример за
т. нар. подход на разделяй и владей (divide and conquer) - за да напишете функция mergesort xs,
която приема списък xs и връща списък с елементите на xs сортирани в нарастващ ред.

Пример:
    mergesort [2, 1, 3, 7, -16, 5] = [-16, 1, 2, 3, 5, 7]
-}
-- Ord a => [a] ... означава, че елементите на списъците са сравними по-между си
-- (или че съществува наредба - Ordering - между тях).
merge :: Ord a => [a] -> [a] -> [a]
merge xs ys
    | null xs   = ys
    | null ys   = xs
    | x <= y    = x:merge xs' ys
    | otherwise = y:merge xs ys' where
        x = head xs; xs' = tail xs
        y = head ys; ys' = tail ys

mergesort :: Ord a => [a] -> [a]
mergesort xs
    | length xs < 2 = xs
    | otherwise     = merge (mergesort hs) (mergesort ts) where
        (hs, ts) = splitAt (length xs `div` 2) xs


-- примери от условията на задачите
main = do
    -- Задача 1.
    print (listLength [])
    print (listLength [1, 2, 3, 4])

    -- Задача 2.
    print (3 `isElementOf` [1, 2, 3, 4])
    print (5 `isElementOf` [1, 2, 3, 4])

    -- Задача 3.
    print (interval 1 4)
    print (interval 2 5)
    print (interval 5 2)

    -- Задача 4.
    print (digits 1234)
    print (digits 1750)

    -- Задача 5.
    print (removeDuplicates [1, 2, 1])
    print (removeDuplicates [1, 3, 7, 3, 5, 1])

    -- Задача 6.
    print (merge [1, 3, 7] [2, 4, 6])
    print (mergesort [2, 1, 3, 7, -16, 5])
