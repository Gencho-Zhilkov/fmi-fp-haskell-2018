{- Tuples, List comprehension, zip/zipWith -}
import Data.Char

{-
I. Вектори (tuples): има три основни разлики между векторите и списъците.
 1. Списъците се пишат с квадратни [], а векторите с кръгли () скоби.

 2. Всички елементи на списък трявбва да са от един и същ тип, т.е. [1, 3.14, "Kasa bira!"]
    ще ви даде грешка. За сметка на това, всеки един от елементите на даден вектор, може да
    бъде от различен тип, т.е. (1, 3.14, "Kasa bira!") е напълно валидно.

 3. За разлика от списъците, броят на елементите на един вектор е част от типа му. Напр.
    типът на двойка цели числа е (Int, Int), а този на тройка числа - (Int, Int, Int).
    За сметка на това, типът на списък от цели числа винаги е [Int], независимо от броя
    на елементите в списъка.


На практика най-често използваните вектори са двойките (pairs). Основните функции върху
тях са fst и snd, които връщат съответно първия и втория елемент на двойката.

    fst (1, 2) -> 1
    snd (1, 2) -> 2


Едно интересно приложение на векторите е присвояването на няколко променливи наведнъж:
    (a, b, c) = (1, 3.14, "Kasa bira!")

което е (почти) същото като:
    a = 1
    b = 3.14
    c = "Kasa bira!"


Няколко примера за типове на вектори:
 - (Ingeger, Bool) - вектор от цяло число и булева стойност.
 - (String, Double) - вектор от символен низ и дробно число.
 - (Integer, [String], Double) - вектор от цяло число, масив от символни низове и дробно число.
 - ((String, Bool), [[Integer]]) - вектор от двойка от символен низ и булева стойност, и масив
   от масиви от цели числа.


II. Още няколко полезни функции върху списъци:
 1. and xs (всички): приема списък от булеви стойности и връща дали всички са True.

 2. or  xs (някой): приема списък от булеви стойности и връща дали някоя от тях е True.

 3. zip xs ys: приема два списъка и връща списък с последователните двойки от елементите им.
    Пример: zip [1 .. 5] "abcde" -> [(1, 'a'), (2, 'b'), (3, 'c'), (4, 'd'), (5, 'e')]

    zip приключва, когато сме взели всички елементи от по-краткия от двата списъка.
    Това ни позволява един от спицъте да бъде "безкраен"
    Пример: zip [1 .. ] "abc" -> [(1, 'a'), (2, 'b'), (3, 'c')]

 4. unzip прави обратното на zip.
    Пример: unzip [(1, 'a'), (2, 'b'), (3, 'c')] -> ([1, 2, 3], "abc")

 5. zipWith f xs ys: приема двуаргументна функция f, както и два списъка xs ys и връща списък
    с резултата (f x y) за всяка двойка (x, y) от zip xs ys (т.е. map + zip на веднъж).

III. List comprehension: [f x | x <- xs, p x]
 List comprehensions са удобна синтактична конструкция за създаване на списъци. Можем да ги
 оприличим на математическия запис за дефиниране на множества. Имаме няколко удобни свойства
 които ще разгледаме с помощта на примери:

  1. Да "прекарваме" елементите на един списък през дадена функция, подобно на функцията map.

    Ако имаме   xs = [1, 2, 3, 4, 5]
    И запишем   [x^2 | x <- xs] 
    Ще получим  [1, 4, 9, 16, 25]

    Можем да тълкуваме записа по следния начин: За всеки елемент от списъка xs, вземи x на квадрат.

  2. Да избираме конкретни елементи от списък по дадено свойство, подобно на функцията filter.

    Ако имаме   xs = [2, 3, 5, 6, 8, 12, 16, 18]
    И запишем   [x | x <- xs, x `mod` 3 == 0, x `mod` 2 == 0] 
    Ще получим  [6, 12, 18]

    За всеки елемент от списъка xs, вземи x, така че x да се дели на 3 и на 2 с остатък 0.

  3. Да комбинираме всички възможни елемента на няколко списъка.

    Ако запишем   [(x, y) | x <- [1, 2, 3], y <- [4, 5]]
    Ще получим    [(1, 4), (1, 5), (2, 4), (2, 5), (3, 4), (3, 5)]

    За всеки елемент x от xs и y от ys вземи всички наредени двойки (вектори, tuples) от вида - (x, y)

  Сега можем да комбинираме всички варианти.
    
    Ако запишем   [firstName ++ " " ++ lastName | firstName <- ["Stamat", "Pesho", "Kiro"], 
                                                  lastName <- ["Georgiev", "Petrov"], 
                                                  length firstName > 4]
    Ще получим    ["Stamat Georgiev", "Stamat Petrov", "Pesho Georgiev", "Pesho Petrov"]

-}

-- Примерна рекурсивна дефиниция на zip
zip' :: [a] -> [b] -> [(a, b)]
zip' (x:xs) (y:ys) = (x, y):zip xs ys
zip' _ _ = []

-- Примерни дефиниции на map, filter и zipWith с list comprehension.
map' :: (a -> b) -> [a] -> [b]
map' f xs = [f x | x <- xs]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f xs = [x | x <- xs, f x]

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f xs ys = [f x y | (x, y) <- zip' xs ys]

-- Декартово произведение на две множества (списъци) посредством list comprehension
cartesianProduct :: [a] -> [b] -> [(a, b)]
cartesianProduct xs ys = [(x, y) | x <- xs, y <- ys]


{-
Задача 1. Дефинирайте функцията enumerate xs, коят прима списък xs и връща списък от
двойки (i, x), където i e индекса на x в xs.

Примери:
    enumerate "abcd" = [(0, 'a'), (1, 'b'), (2, 'c'), (3, 'd')]
-}
enumerate :: Integral b => [a] -> [(b, a)]
enumerate xs = undefined


{-
Задача 2.
а). Дефинирайте функцията allCapsAtOddPos str, която приема символен низ str и връща дали
    всички символи на нечетна позиция са главни букви.

Примери:
    allCapsAtOddPos "AAAA" = True
    allCapsAtOddPos "aAaA" = True
    allCapsAtOddPos "aAaa" = False

б). Дефинирайте функцията anyDigitAtEvenPos str, която приема символен низ str и връща дали
    някой от символите на четна позиция е цифра.

Примери:
    anyDigitAtEvenPos "1Asw" = True
    anyDigitAtEvenPos "q12w" = True
    anyDigitAtEvenPos "q1w2" = False
-}
-- and, за да проверим дали всички са верни
allCapsAtOddPos :: String -> Bool
allCapsAtOddPos str = undefined

-- оr, за да проверим дали някой е верен
anyDigitAtEvenPos :: String -> Bool
anyDigitAtEvenPos str = undefined


{-
Задача 3. Дефинирайте функцията indices x xs, която връща всички индекси на елементи
от списъка xs, чиято стойност е равна на x.

Примери:
    indices 1 [1, 2, 3, 1, 4] = [0, 3]
    indices 1 [] = []
-}
indices :: Eq a => a -> [a] -> [Int]
indices x xs = undefined


{-
Задача 4. Просто пренареждане: Дефинирайте функцията primeReorder xs, която получава
списък xs и връща нов списък ys. В началото на ys трябва да са елементите, които са били
с индекс просто число в xs. След тях трябва да са всички останали. Индексирането в xs,
започва от 2.

Примери:
    primeReorder [2,3,4,5,6] = [2,3,5,4,6]
    primeReorder "abcd" = "abdc"
-}
isPrime :: Integer -> Bool
isPrime n = n > 1 && null [d | d <- [2 .. sqrtn], n `mod` d == 0] where
    sqrtn = floor (sqrt (fromIntegral n))

primeReorder :: [a] -> [a]
primeReorder xs = undefined


{-
Задача 5: Дефинирайте функцията lastIndex x xs, която приема 2 аргумента - списък от числа и
число и връща индексa (0-базиран) на последното срещане на числото в списъка. Ако числото не
се среща в списъка функцията връща грешка.

Примери:
    lastIndex 1 [1, 2, 7, 1, 5, 4] = 3
    lastIndex 7 [3, 1, 7, 5] = 2
    lastIndex 1 [] = error "not in list"
    lastIndex 3 [2, 4, 7] = error "not in list"
-}
lastIndex :: Integer -> [Integer] -> Integer
lastIndex x xs = undefined


{-
Задача 6. Брой на повторенията на най-малкото число в списъка: дефинирайте функцията
countMin xs, която намира броя на срещанията на най-малкия елемент на списъка xs в
него. Списъкът xs е несортиран и e съставен само от положителни цели числа.

Примери:
    countMin [1, 2, 1, 1, 5, 3] = 3
    countMin [3, 4, 2] = 1
    countMin [] = 0
-}
countMin :: [Integer] -> Int
countMin xs = undefined


{-
Задача 7. Хетерограми: дефинирайте функцията isHeterogram str, която проверява дали
символният низ str e хетерограма. Хетерограма се нарича символен низ, в който всеки
символ се среща само по веднъж.

Примери:
    isHeterogram "abcd" = True
    isHeterogram "abbd" = False
-}
isHeterogram :: String -> Bool
isHeterogram str = undefined


{-
Задача 8.
a). Дефинирайте функцията vectSum xs ys, която връща сбора на векторите xs и ys.

Примери:
    vectSum [1, 2, 3] [4, 5, 6] = [5, 7, 9]

б). Дефинирайте функцията scalarProd xs ys, която връща скаларното произведение на
    векторите xs и ys.

Примери:
    scalarProd [1, 2, 3] [4, 5, 6] = 32
-}
vectSum :: Num a => [a] -> [a] -> [a]
vectSum xs ys = undefined

scalarProd :: Num a => [a] -> [a] -> a
scalarProd xs ys = undefined


-- примери от условията
main :: IO ()
main = do
    -- Задача 1.
    print (enumerate "abcd")

    -- Задача 2.
    print (allCapsAtOddPos "AAAA")
    print (allCapsAtOddPos "aAaA")
    print (allCapsAtOddPos "aAaa")
    print (anyDigitAtEvenPos "1Asw")
    print (anyDigitAtEvenPos "q12w")
    print (anyDigitAtEvenPos "q1w2")

    -- Задача 3.
    print (indices 1 [1, 2, 3, 1, 4])
    print (indices 1 [])

    -- Задача 4.
    print (primeReorder [2,3,4,5,6])
    print (primeReorder "abcd")

    -- Задача 5.
    print (lastIndex 1 [1, 2, 7, 1, 5, 4])
    print (lastIndex 7 [3, 1, 7, 5])

    -- Задача 6.
    print (countMin [1, 2, 1, 1, 5, 3])
    print (countMin [3, 4, 2])
    print (countMin [])

    -- Задача 7.
    print (isHeterogram "abcd")
    print (isHeterogram "abbd")

    -- Задача 8.
    print (vectSum [1, 2, 3] [4, 5, 6])
    print (scalarProd [1, 2, 3] [4, 5, 6])
