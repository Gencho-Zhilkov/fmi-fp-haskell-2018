{- Рекурсивни функции върху числа -}

{- Примери -}
-- Функцията isPrime n, която приема цялото число n и проверява дали то е просто.
isPrime :: Integer -> Bool
isPrime n = n >= 2 && helper 2 where
    helper i
        | i > sqrtn         = True
        | n `mod` i == 0    = False
        | otherwise         = helper (i + 1) where
            sqrtn = floor (sqrt (fromIntegral n))

-- функцията isPerfectNumber n, която проверява дали цялото число n е перфектно.
isPerfectNumber :: Integer -> Bool
isPerfectNumber n = n == sumDivisors 1 0 where
    sumDivisors i res
        | i >= n            = res
        | n `mod` i == 0    = sumDivisors (i + 1) (res + i)
        | otherwise         = sumDivisors (i + 1) res


{- Задачи -}
{-
Задача 1. Напишете оператор n ## k, който приема n > 0 и k >= 0 и връща сумата от всяка цифра
на n повдигната на степен k.

Примери:
    12 ## 2 = 1 ^ 2 + 2 ^ 2 = 1 + 4   = 5
    17 ## 3 = 1 ^ 3 + 7 ^ 3 = 1 + 343 = 344
-}
(##) :: Integer -> Integer -> Integer   -- Когато пишем типа на оператора, ограждаме името му със скоби.
n ## k = undefined


{-
Задача 2. Да се дефинира функцията isNarcissistic n, която приема като аргумент цялото положително
число n и връща дали то е нарцистично. Нарцистични се наричат числата, които са равни на сбора на
цифрите си (в десетична бройна система), всяка повдигната на степен броя на цифрите на числото.

Пример за такова число е 153, тъй като 1 ^ 3 + 5 ^ 3 + 3 ^ 3 = 1 + 125 + 27 = 153.
-}
isNarcissistic :: Integer -> Bool
isNarcissistic n = undefined


{-
Задача 3. Серия на Тейлър: дефинирайте функцията taylorLog n x, която примема целочисления аргумент n и
реалното число x и връща сумата от първите n члена от реда на Тейлър на функцията ln(1 + x), дефиниран като:

ln(1 + x) = Sum{k = 0 ... n} (- 1) ^ k * x ^ (k + 1) / (k + 1)
-}
taylorLog :: Int -> Double -> Double
taylorLog n x = undefined


{-
Задача 4. Вавилонски метод за пресмятане на квадратен корен: Да се дефинира функцията
babylonianSquareRoot square guess epsilon, която използва вавилонския метод, за да
пресметне квадратния корен на числото square с точност epsilon, започвайки за целта от
първоначалната стойност guess.

Вавилонският метод се състои от следните стъпки:

1. Започва се с произволно положително число guess. Колкото по-близко е то до квадратния
корен на square, толкова по-малко итерации ще са нужни за да се постигне желаната точност.

2. Замества се guess със средното аритметичното на guess и square/guess.

3. Повтаря се стъпка 2, използвайки полученото средно аритметично като нова стойност за
guess, докато не се достигне желаната точност (epsilon).
-}
babylonianSquareRoot :: Double -> Double -> Double -> Double
babylonianSquareRoot square guess epsilon = undefined


{-
Задача 5. Да се дефинира функцията sumPrimeDivisors n, която връща сбора на всички
прости делители на числото n.

Примери:
    sumPrimeDivisors (2 * 3 * 5 * 7) -> 2 + 3 + 5 + 7 = 17
    sumPrimeDivisors 997 -> 997 (тъй като 997 е просто число)
-}
sumPrimeDivisors :: Integer -> Integer
sumPrimeDivisors n = undefined


-- примери от условията на задачите
main = do
    -- Задача 1.
    print (12 ## 2)
    print (17 ## 3)

    -- Задача 2.
    print (isNarcissistic 153)
    print (isNarcissistic 28)

    -- Задача 3.
    -- двете числа трябва да са сравнително близки ...
    print (taylorLog 100 0.5, log 1.5)
    print (taylorLog 100 1, log 2)

    -- Задача 4.
    -- двете числа трябва да са сравнително близки ...
    print (babylonianSquareRoot  2 1 1e-4, sqrt  2)
    print (babylonianSquareRoot 10 1 1e-6, sqrt 10)

    -- Задача 5.
    print (sumPrimeDivisors (2 * 3 * 5 * 7))
    print (sumPrimeDivisors 997)
