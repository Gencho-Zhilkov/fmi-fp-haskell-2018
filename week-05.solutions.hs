{- Символи. Символни низове. Рекурсия върху символни низове. Pattern matching. -}
import Data.Char -- модул с полезни функции върху символи
import Data.List

{-
1. Символи: символите в Haskell са от тип Char (подобен на типа char в С/С++)

    Полезни функции върху символи:

    ord c - връща ASCII кода на символа c. Пример: ord 'q' = 113
    chr n - връща символа с ASCII код n. Пример: chr 113 = 'q' 

    import Data.Char ви позволява да използвате следните полезни функции:

    toUpper c - обръща буквата c в главна.          
    Пример: toUpper 'Q' = 'Q';      toUpper 'q' = 'Q';      toUpper '1' = '1'
    
    toLower c - обръща буквата c в малка.           
    Пример: toLower 'Q' = 'q';      toLower 'q' = 'q';      toLower '1' = '1'
    
    isUpper c - проверява дали буквата c e главна.
    Пример: isUpper 'Q' = True;     isUpper 'q' = False;    isUpper '1' = False
    
    isLower c - проверява дали буквата c e малка.   
    Пример: isLower 'Q' = False;    isLower 'q' = True;     isLower '1' = False
    
    isAlpha c - проверява дали символът c е буква.  
    Пример: isAlpha 'Q' = True;     isAlpha 'q' = True;     isAlpha '1' = False
    
    isDigit c - проверява дали символът c e цифра.  
    Пример: isDigit 'Q' = False;    isDigit 'q' = False;    isDigit '1' = True
    
    isSpace c - проверява дали символът c e интервал.   
    Пример: isSpace ' ' = True;     isSpace '\t' = True;    isSpace 'q' = False
    
    isAlphaNum c - проверява дали символът c е буква или цифра.
    Пример: isAlphaNum ';' = False; isAlphaNum 'q' = True;  isAlphaNum '1' = True


2. Символни низове: символният низ в Haskell е просто списък от символи, т.е. String = [Char].
   Следователно всички функции работещи върху списъци, работят и върху символни низове!


3. Функциите read и show:
    show a - връща стринговата репрезентация на а.
    Пример:
        show 123        = "123"
        show "123"      = "\"123\""
        show [1, 2, 3]  = "[1,2,3]"

    read s - конвертира символния низ s до стойност.
    Пример:
        read "123" :: Integer           = 123
        read "[1,2,3]" :: [Integer]     = [1, 2, 3]

4. Pattern matching (Съпоставяне по образец):
    В съпоставянето по образец, се опитваме да съпоставяме стойности на входни данни
    по обределени образци, и ако желаем - bind-ваме (присвояваме) стойностите на
    успешните образци. Какво означава това? Често имаме функции от вида:

        isDayInWeekend :: Int -> Bool
        isDayInWeekend day
            | day == 6  = True
            | day == 7  = True
            | otherwise = False

    Като използваме pattern matching можем да опишем тази функция със следните равенства:

        isDayInWeekend :: Int -> Bool
        isDayInWeekend 6 = True
        isDayInWeekend 7 = True
        isDayInWeekend _ = False

    Равенствата изглеждат като дефиниции на функцията за различни случаи. Те се проверяват 
    последователно докато не хванем някой от случаите. Символът '_' (наречен wildcard) означава,
    че match-ваме винаги, т.е. каквато и стойност да бъде подадена ще бъде съпоставена правилно.
    Както виждаме в горния случай, можем да match-ваме по конкретни стойности (литерали).
    Можем също така да съпоставяме променливи (например x) както сме правили досега с дефинициите
    на нашите функции. Списъците също имат удобни образци по които можем да ги съпоставяме:
        
        sum :: [Integer] -> Integer
        sum [] = 0
        sum (x:xs) = x + sum xs

        dropThree :: [a] -> [a]
        dropThree [] = []
        dropThree (_:[]) = []
        dropThree (_:_:[]) = []
        dropThree (_:_:_:xs) = xs

        или просто
        
        dropThree :: [a] -> [a]
        dropThree (_:_:_:xs) = xs
        dropThree _ = []

    За по-подробна информация разгледайте презентациите по лекциите.
-}


{-
Пример 1. Да се дефинира фунцкцията whisper str, която обръща в малки букви 
всички символи на низа str.

Пример: 
    whisper "banaNA" = whisper "BAnaNA" = "banana"
-}
whisper :: String -> String
whisper "" = ""
whisper (x:xs) = toLower x : whisper xs

whisper' :: String -> String
whisper' str =
    if null str
    then ""
    else toLower (head str) : whisper' (tail str)



{-
Пример 2. Да се дефинира функцията removeSpaces str, която премахва всички интервали
от символния низ str.

Пример: 
    removeSpaces "The Sound And The Fury" = "TheSoundAndTheFury"
-}
removeSpaces :: String -> String
removeSpaces "" = ""
removeSpaces (x:xs) = if isSpace x then removeSpaces xs else x : removeSpaces xs

removeSpaces' :: String -> String
removeSpaces' str
    | null str  = ""
    | otherwise = if isSpace x then removeSpaces' xs else x : removeSpaces' xs
    where
        x  = head str
        xs = tail str


{-
Задача 1. Дефинирайте функцията shout str, която обръща в главни всички букви 
на символния низ str

Пример: 
    shout "banana" = shout "bAnaNA" = shout "BanAna" = "BANANA"
-}
shout :: String -> String
shout str =
    if null str
    then ""
    else toUpper (head str) : shout (tail str)


{-
Задача 2. Напишете функцията capitalise str, която обръща само първата буква в 
главна, а останалите в малки.

Пример: 
    capitalise "banana" = capitalise "baNAna" = "Banana"
-}
capitalise :: String -> String
capitalise "" = ""
capitalise (x:xs) = toUpper x : whisper xs

capitalise' :: String -> String
capitalise' str =
    if null str
    then ""
    else toUpper (head str) : whisper (tail str)


{-
Задача 3. Напишете функцията switchCaps str, която обръща малките букви в големи,
а големите в малки.

Пример: 
    switchCaps "baNaNA" = "BAnAna"
-}
switchCaps :: String -> String
switchCaps "" = ""
switchCaps (x:xs) =
    if isUpper x
    then toLower x : switchCaps xs
    else toUpper x : switchCaps xs

switchCaps' :: String -> String
switchCaps' str
    | null str = ""
    | isUpper x = toLower x : switchCaps' xs
    | otherwise = toUpper x : switchCaps' xs
    where
        x  = head str
        xs = tail str


{-
Задача 4. Дефинирайте функциите encrypt str n и decrypt str n, които имплементират един от най-простите
начини за кодиране на информация - Ceasar Cipher. Той работи като измества всяка буква 
от азбуката с буквата която е n позиции след нея. Ако някоя от буквите "превърти", т.е. 
задмине последната буква от азбуката, то тогава продължаваме с буквите в началото на азбуката.

Пример: 
    encrypt "ABC" 1 = "BCD"
    encrypt "AXYZ" 2 = "CZAB"
    decrypt "BCD" 1 = "ABC"
    decrypt "CZAB" 2 = "AXYZ"
-}
encrypt :: String -> Int -> String
encrypt "" _ = ""
encrypt (x:xs) n = shifted : encrypt' xs n
    where
        shifted = chr $ (ord x - ord 'A' + n) `mod` 26 + ord 'A'

decrypt :: String -> Int -> String
decrypt str n = encrypt str (-n)

encrypt' :: String -> Int -> String
encrypt' str n =
    if null str
    then ""
    else shifted : encrypt' xs n
    where
        shifted = chr $ (ord x - ord 'A' + n) `mod` 26 + ord 'A'
        x  = head str
        xs = tail str


{-
Задача 5. Напишете функцията joinWords c strs, която слива няколко думи в една,
използвайки за разделител символ c.

Примери:
    joinWords ' ' ["The", "Sound", "of", "Silence"] = "The Sound of Silence"
    joinWords ',' ["One", "Two", "Three", "Four"] = "One,Two,Three,Four"
-}
joinWords :: Char -> [String] -> String
joinWords _ [] = ""
joinWords _ (x:[]) = x
joinWords c (x:xs) = x ++ [c] ++ joinWords c xs

joinWords' :: Char -> [String] -> String
joinWords' c strs
    | null strs = ""
    | length strs == 1 = head strs
    | otherwise = head strs ++ [c] ++ joinWords' c (tail strs)


{-
Задача 6. Трансформация на Бъроус-Уилър (Burrows-Wheeler transform)

Трансформацията на Бъроус-Уилър, е трансформация върху символни низове, която има
интересното свойство да групира еднаквите символи в низа близо един до друг.
Поради тази причина, тя се използвана като предварителна стъпка в алгоритмите за
компресия на данни, както и в биоинформатиката.

Алгоритъмът за намирането и е следния:
1. Генерираме всички ротации на входния низ, което ни дава списък от низове.
2. Сортираме списъка лексикографски. (Използвайте функцията sort от Data.List)
3. Взимаме последния символ от всеки от редовете.

Пример:
    "BANANA" -- rotations --> [ "BANANA", -- sort --> [ "ABANAN", -- last symbol --> "NNBAAA"
                                "ANANAB",               "ANABAN",
                                "NANABA",               "ANANAB",
                                "ANABAN",               "BANANA",
                                "NABANA",               "NABANA",
                                "ABANAN" ]              "NANABA" ]

a). Напишете функцията rotate n str, която ротира низа str с n позиции наляво.

    Пример: rotate 1 "abc" = "bca"
            rotate 2 "abc" = "cab"

б). Напишете функцията rotations str, която генерира всички ротации на str.

    Пример: rotations "abc" = ["abc", "bca", "cab"]

в). Напишете функцията bwt str, която приема низа str, и връща неговата трансформация
    на Бъроус-Уилър.

    Пример: bwt "BANANA" = "NNBAAA"
-}
rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate n (x:xs) = rotate (n - 1) (xs ++ [x])

rotate' :: Int -> [a] -> [a]
rotate' n xs
    | null xs = []
    | n == 0  = xs
    | otherwise = rotate' (n - 1) (tail xs ++ [head xs])

rotations :: [a] -> [[a]]
rotations xs = helper 0 where
    helper i =
        if i == length xs
        then []
        else rotate i xs : helper (i + 1)

bwt :: Ord a => [a] -> [a]
bwt xs = helper (sort $ rotations xs) where
    helper [] = []
    helper (ys:yss) = lastElementSublist : helper yss where
        lastElementSublist = head (reverse ys)


-- примери от условията
main :: IO()
main = do
    -- Пример 1.
    print (whisper "banaNA")
    print (whisper "BAnaNA")

    -- Пример 2.
    print (removeSpaces "The Sound And The Fury")

    -- Задача 1.
    print (shout "banana")
    print (shout "bAnaNA")
    print (shout "BanAna")

    -- Задача 2.
    print (capitalise "banana")
    print (capitalise "baNAna")

    -- Задача 3.
    print (switchCaps "baNaNA")

    -- Задача 4.
    print (encrypt "AXYZ" 2)
    print (decrypt "CZAB" 2)

    -- Задача 5.
    print (joinWords ' ' ["The", "Sound", "of", "Silence"])
    print (joinWords ',' ["One", "Two", "Three", "Four"])

    -- Задача 6.
    print (rotate 1 "abc")
    print (rotate 2 "abc")
    print (rotations "abc")
    print (rotations "BANANA")
    print (bwt "BANANA")
