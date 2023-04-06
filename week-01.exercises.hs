-- Едноредовите коментари в Haskell се пишат така.

{-
A многоредовите коментари изглеждат ето така.

-- има същата роля като // в C/C++.
{- ... -} е същото като /* ... */ в C/C++.
-}


{-
Основни типове за днес:

1. Булеви стойности: Булевите стойности в Haskell са от тип Bool
(подобно на bool в С/С++).
Могат да приемат стойностите True или False (true и false в С/С++).

    Операции върху булеви променливи:
    &&  - логическо И   (същото като С/С++)
    ||  - логическо ИЛИ (същото като С/С++)
    not - логическо НЕ  (! в С/С++)


2. Числа: засега е важно да знаете два основни типа:
    а). Челочислени стойности: използвайте типа Integer,
    тъй като той е с неограничена точност за разлика от Int.

    б). Дробни числа: използвайте типа Double,
    тъй като е с по-добра точност от типа Float.

    Познатите ви от С/С++ аритметични операции работят почти както се очаква.
    За разлика от С/С++ НЕ може да прилагате аритметични
    операции върху числа от различен тип.

    - За да конвертирате цяло към дробно число, използвайте функцията
    fromIntegral.

    - За да конвертирате дробно към цяло число, използвайте функциите:
    ceiling  -- ceiling 5.1 = 6;    ceiling 5.6 = 6;
    floor    -- floor   5.1 = 5;    floor   5.6 = 5;
    round    -- round   5.1 = 5;    round   5.6 = 6;

    - Целочислено делене и остатък:
    div 5 2 == 5 `div` 2 = 2
    mod 5 2 == 5 `mod` 2 = 1
    Вторият запис се счита за "по-четим" и е за предпочитане пред първия.
-}


{- Примери -}
-- 1. Дефиниране на функцията add3 x y z, която приема числата x, y и z, и връща сбора им.
add3 :: Integer -> Integer -> Integer -> Integer
add3 x y z = x + y + z

-- 2. Условни изрази: max2 x y, която приема аргументи x и y, и връща по-големия от тях.
-- Вариант 1. if-then-else (подобно на ?: в C++)
max2 :: Integer -> Integer -> Integer
max2 x y =
    if x >= y
        then x
        else y

-- Вариант 2. guards (подобно на switch в C++)
max2' x y
    | x >= y    = x
    | otherwise = y

-- 3. Композиция на функции: max3 x y z, която приема аргументи x, y и z, и връща най-големия от тях.
max3 x y z = max2 (max2 x y) z


{- ЗАДАЧИ -}
{-
Задача 1. Да се дефинира функцията isInside x a b, която проверява дали числото x се намира
в затворения интервал [a .. b].

ПримерИ:
    isInside  7 1 10    -> True
    isInside 13 1 10    -> False
    isInside (-5) 1 10  -> False
-}
isInside :: Integer -> Integer -> Integer -> Bool
isInside x a b = undefined


{-
Задача 2. Да се дефинира функцията isLeapYear year, която проверява дали годината year
е високосна.

ПримерИ:
    isLeapYear 1984 -> True
    isLeapYear 1995 -> False
    isLeapYear 2000 -> True
    isLeapYear 2100 -> False
-}
isLeapYear :: Integer -> Bool
isLeapYear year = undefined


{-
Задача 3. Да се дефинира функцията numberOfDays month year, която връща броя на дните
на месец month за годината year.

ПримерИ:
    numberOfDays 1 1995 -> 31
    numberOfDays 1 2000 -> 31
    numberOfDays 2 1995 -> 28
    numberOfDays 2 2000 -> 29
-}
numberOfDays :: Integer -> Integer -> Integer
numberOfDays month year = undefined


{-
Задача 4. Да се дефинира функцията isValidDate day month year, която връща дали датата
(day, month, year) e валидна.

ПримерИ:
    isValidDate 31 1 1995 -> True
    isValidDate 29 2 1995 -> False
    isValidDate 29 2 2000 -> True
    isValidDate 29 2 2100 -> False
-}
isValidDate :: Integer -> Integer -> Integer -> Bool
isValidDate day month year = undefined


{-
Задача 5. Да се дефинира функцията distance x1 y1 x2 y2, която връща разстоянието между
точките (x1, y1) и (x2, y2).

ПримерИ:
    distance 0 0 1 1 -> 1.41421...
    distance 0 1 1 0 -> 1.41421...
    distance 0 0 0 1 -> 1.0
-}
distance :: Double -> Double -> Double -> Double -> Double
distance x1 y1 x2 y2 = undefined


{-
Задача 6. Да се дефинира функцията countIntersections x1 y1 r1 x2 y2 r2, която връща броя на
пресечните точки между 2-те окръжности с центрове (x1, y1) и (x2, y2) и радиуси r1 и r2.

ПримерИ:
    countIntersections 0 0 2 1 1 1 -> 2
    countIntersections 0 0 3 1 1 1 -> 0
    countIntersections 0 0 1 0 2 1 -> 1
-}
countIntersections :: Double -> Double -> Double -> Double -> Double -> Double -> Integer
countIntersections x1 y1 r1 x2 y2 r2 = undefined


{-
Задача 7. Редица на Фибоначи: дефинирайте функцията fib n, която връща n-тото число от редицата на
Фибоначи, дефинирана като:

    fib(0) = 0
    fib(1) = 1
    fib(n) = fib(n - 1) + fib(n - 2)

ПримерИ:
    fib 3 -> 2
    fib 8 -> 21
-}
-- `Рекурсивно` решение:
fib :: Integer -> Integer
fib n = undefined


-- Упражненията ще съдържат main функция изброяваща примерни извиквания на функциите от задачите.
main :: IO()
main = do
    -- Задача 1.
    print (isInside  7 1 10)
    print (isInside 13 1 10)
    print (isInside (-5) 1 10)

    -- Задача 2.
    print (isLeapYear 1984)
    print (isLeapYear 1995)
    print (isLeapYear 2000)
    print (isLeapYear 2100)

    -- Задача 3.
    print (numberOfDays 1 1995)
    print (numberOfDays 1 2000)
    print (numberOfDays 2 1995)
    print (numberOfDays 2 2000)

    -- Задача 4.
    print (isValidDate 31 1 1995)
    print (isValidDate 29 2 1995)
    print (isValidDate 29 2 2000)
    print (isValidDate 29 2 2100)

    -- Задача 5.
    print (distance 0 0 1 1)
    print (distance 0 1 1 0)
    print (distance 0 0 0 1)

    -- Задача 6.
    print (countIntersections 0 0 1 1 1 1)
    print (countIntersections 0 0 3 1 1 1)
    print (countIntersections 0 0 1 0 2 1)

    -- Задача 7.
    print (fib 0)
    print (fib 1)
    print (fib 2)
    print (fib 3)
    print (fib 4)
    print (fib 5)
    print (fib 6)
    print (fib 7)
    print (fib 8)
