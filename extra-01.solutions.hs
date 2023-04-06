lastDigit :: Integer -> Integer
lastDigit n = n `mod` 10

firstDigits :: Integer -> Integer
firstDigits n = n `div` 10

numDigits :: Integer -> Integer
numDigits n =
    if n < 10 
    then 1
    else 1 + numDigits (firstDigits n)


{-
Задача. 1. Да се напише предикат isAscending, който връща истина, ако цифрите на
дадено естествено число са в нарастващ ред от първата към последната.

    isAscending 5    -> True
    isAscending 121  -> False
    isAscending 122  -> True
    isAscending 123  -> True
-}
isAscending :: Integer -> Bool
isAscending n =
    if n < 10
    then True
    else (nMinusFirst <= nth) && isAscending (firstDigits n) where
        nMinusFirst = lastDigit (firstDigits n)
        nth = lastDigit n

{-
Задача. 2. Да се напише функция countOccurrences, намираща броя на срещанията на дадена
цифра d в записа на число n.

    countOccurrences 5 5      -> 1
    countOccurrences 5 25525  -> 3
    countOccurrences 5 12346  -> 0
-}
countOccurrences :: Integer -> Integer -> Integer
countOccurrences d n
    | n < 10 = if d == n then 1 else 0
    | lastDigit n == d = 1 + countOccurrences d (firstDigits n)
    | otherwise = countOccurrences d (firstDigits n)

{-
Задача. 3. Напишете функция, която за дадено неотрицателно цяло число проверява
дали на всяка четна позиция в десетичния запис на числото стои нечетна цифра. 
Нека старшата (най-лявата) цифра на числото има позиция 0.

    evenPosOddDigits 5    -> True
    evenPosOddDigits 6    -> False
    evenPosOddDigits 123  -> True
    evenPosOddDigits 122  -> False
-}
evenPosOddDigits :: Integer -> Bool
evenPosOddDigits n
    | lastIndex == 0 = odd (lastDigit n)
    | even lastIndex && even (lastDigit n) = False
    | otherwise = evenPosOddDigits (firstDigits n)
    where
        lastIndex = numDigits n - 1

evenPosOddDigits' :: Integer -> Bool
evenPosOddDigits' n = helper (numDigits n - 1) n where
    helper pos num
        | pos < 0 = True
        | even pos && even (lastDigit num) = False
        | otherwise =  helper (pos - 1) (firstDigits num)

{-
Задача 4. Да се дефинира функция която взима числото, което се образува
от последните n цифри на числото m.

    takeNFromEnd 1 123  -> 3
    takeNFromEnd 2 123  -> 23
    takeNFromEnd 3 123  -> 123
-}
takeNFromEnd :: Integer -> Integer -> Integer
takeNFromEnd n m = 
    if n == 0
    then 0
    else lastDigit m + 10 * takeNFromEnd (n - 1) (firstDigits m) 

takeNFromEnd' :: Integer -> Integer -> Integer
takeNFromEnd' n m = helper 0 0 m where
    helper result position number =
        if position == n
        then result
        else helper (result + 10 ^ position * lastDigit number)
                    (position + 1)
                    (firstDigits number)

{-
Задача. 5. Да се дефинира предикат isAutomorphic, който приема число n и
проверява дали n^2 завършва с цифрите на n.

    isAutomorphic 2   -> False
    isAutomorphic 5   -> True
    isAutomorphic 25  -> True
    isAutomorphic 26  -> False
-}

isAutomorphic :: Integer -> Bool
isAutomorphic n = lastFewDigits (n ^ 2) == n where
    lastFewDigits m = takeNFromEnd (numDigits n) m

{-
Задача. 6. Да се дефинира функция, която намира броя на срещанията
на многоцифрено число в записа на число n. (Като в задача 2, но не само
с едноцифрени числа.)

    print $ countOccurrences' 11 1111    -> 3
    print $ countOccurrences' 25 125625  -> 2
    print $ countOccurrences' 300 23     -> 0
-}
countOccurrences' :: Integer -> Integer -> Integer
countOccurrences' d n
    | numDigits n == numDigits d = if n == d then 1 else 0
    | lastMatch == d = 1 + countOccurrences' d (firstDigits n)
    | otherwise = countOccurrences' d (firstDigits n)
    where
        lastMatch = takeNFromEnd (numDigits d) n


main = do
    print $ isAscending 5
    print $ isAscending 121
    print $ isAscending 122
    print $ isAscending 123

    print $ countOccurrences 5 5
    print $ countOccurrences 5 25525
    print $ countOccurrences 5 12346

    print $ evenPosOddDigits 5
    print $ evenPosOddDigits 6
    print $ evenPosOddDigits 123
    print $ evenPosOddDigits 122

    print $ takeNFromEnd 1 123
    print $ takeNFromEnd 2 123 
    print $ takeNFromEnd 3 123 

    print $ isAutomorphic 2
    print $ isAutomorphic 5
    print $ isAutomorphic 25
    print $ isAutomorphic 26

    print $ countOccurrences' 11 1111
    print $ countOccurrences' 25 125625
    print $ countOccurrences' 300 23
    
