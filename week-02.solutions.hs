{- Рекурсивни функции върху числа -}

{- Примери -}
-- 1. Функция, която връща сумата на числата от a до b.
-- Вариант 1. стандартна рекурсия
sum' :: Integer -> Integer -> Integer
sum' a b
    | a > b     = 0                 -- базов случай / дъно на рекурсията
    | otherwise = a + sum' (a + 1) b -- общ случай

-- Вариант 2. опашкова рекурсия:
sum'' :: Integer -> Integer -> Integer
sum'' a b = helper a 0 where
    helper i res =
        if i > b
            then res                        -- в дъното връшаме натрупания резултат (акумулатор)
            else helper (i + 1) (res + i)   -- в общия случай викаме рекурсивно функцията със стойностите от слвдващата "итерация"


-- 2. Функция, която връща факториела на числото n.
-- Вариант 1. стандартна рекурсия
factorial' :: Integer -> Integer
factorial' n =
    if n == 0
    then 1                          -- базов случай / дъно на рекурсията
    else n * factorial' (n - 1)      -- общ случай

factorial'' :: Integer -> Integer
factorial'' n = helper 1 1 where
    helper i res
        | i > n     = res
        | otherwise = helper (i + 1) (res * i)


{- Задачи -}
{-
Задача 1. Най-голям общ делител: Напишете функцията findGCD a b, която приема 2 цели
числа а и b и връща най-големия им общ делител. За целта може да използвате
алгоритъма на Евклид, който гласи, че най-големият общ делител на а и b е
най-големия общ делител на b и а остатък b ако а не се дели на b или b, ако се.

Не можете да използвате функцията gcd от Прелюдията, която прави точно това.

Примери:
    findGCD 2 1     -> 1
    findGCD 5 15    -> 5
    findGCD 15 35   -> 5
-}
findGCD :: Integer -> Integer -> Integer
findGCD a b
    | m == 0    = b
    | otherwise = findGCD b m where
        m = a `mod` b


{-
Задача 2. Напишете функция, която обръща цифрите на числото n.

Примери:
    reverseDigits 0     -> 0
    reverseDigits 5     -> 5
    reverseDigits 12345 -> 54321
    reverseDigits 1212  -> 2121
    reverseDigits 1000  -> 1
-}
reverseDigits :: Integer -> Integer
reverseDigits n = helper n 0 where
    helper i res =
        if i == 0
            then res
            else helper (i `div` 10) (10 * res + i `mod` 10)


{-
Задача 3. Прости числа: дефинирайте функцията isPrime n, която приема цялото число n
и проверява дали то е просто.

Примери:
    isPrime 1   -> False
    isPrime 2   -> True
    isPrime 3   -> True
    isPrime 52  -> False
    isPrime 71  -> True
-}
isPrime :: Integer -> Bool
isPrime n = n >= 2 && helper 2 where
    helper i
        | i > sqrtn         = True
        | n `mod` i == 0    = False
        | otherwise         = helper (i + 1) where
            sqrtn = floor (sqrt (fromIntegral n))


{-
Задача 4. Дефинирайте функцияга sumPrimes a b, която връща сумата на всички прости числа
в интервала от a до b.

Примери:
    sumPrimes 1 10  -> 17
    sumPrimes 2 5   -> 10
-}
sumPrimes :: Integer -> Integer -> Integer
sumPrimes a b
    | a > b     = 0
    | isPrime a = a + sumPrimes (a + 1) b
    | otherwise = sumPrimes (a + 1) b


{-
Задача 5. Да се дефинира функцията isPerfectNumber n, която приема целочисления аргумент n и
връща дали той е перфектно число. Перфектно число се нарича всяко чяло число, равно на сбора на
собствeните си делители.

Примери:
    isPerfectNumber 6    -> True
    isPerfectNumber 8    -> False
    isPerfectNumber 28   -> True
    isPerfectNumber 500  -> False
-}
isPerfectNumber :: Integer -> Bool
isPerfectNumber n = n == sumDivisors 1 0 where
    sumDivisors i res
        | i >= n            = res
        | n `mod` i == 0    = sumDivisors (i + 1) (res + i)
        | otherwise         = sumDivisors (i + 1) res


-- примери от условията на задачите
main = do
    -- Задача 1.
    print (findGCD 2 1)
    print (findGCD 5 15)
    print (findGCD 15 35)

    -- -- Задача 2.
    print (reverseDigits 0)
    print (reverseDigits 5)
    print (reverseDigits 12345)
    print (reverseDigits 1212)
    print (reverseDigits 1000)

    -- -- Задача 3.
    print (isPrime 1)
    print (isPrime 2)
    print (isPrime 3)
    print (isPrime 52)
    print (isPrime 71)

    -- -- Задача 4.
    print (sumPrimes 1 10)
    print (sumPrimes 2 5)

    -- -- Задача 5.
    print (isPerfectNumber 6)
    print (isPerfectNumber 8)
    print (isPerfectNumber 28)
    print (isPerfectNumber 500)
