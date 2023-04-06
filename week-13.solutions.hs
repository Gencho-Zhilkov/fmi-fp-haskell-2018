import Data.List 

{- Дефиниция на типа Tree a описващ произволно двоично дърво. -}
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)

-- помощна ф-я, връщаща стойностите на ниво k.
nodesAtLevel :: (Eq b, Num b) => Tree a -> b -> [a]
nodesAtLevel Empty _ = []
nodesAtLevel (Node x _ _) 0 = [x]
nodesAtLevel (Node _ left right) n = nodesAtLevel left (n - 1) ++ nodesAtLevel right (n - 1)


{-
Задача 1.
а). Напишете функцията distance f xys, която приема едноместна функция f
    и списък от двойки (x, y) и връща сбора от разстоянията |y - f(x)| между f(x)
    и y, за всички елементи от списъка (в случая |a| означава абсолютната стойност
    на числото а).

    Ще наричаме резултата на тази функция, разстояние на функцията f от xys.

Примери:
    distance (\x -> x ^ 2) [(1, 1), (2, 4), (3, 9), (4, 15)] = 1
    distance (\x -> x + 1) [(1, 1), (2, 4), (3, 9), (4, 15)] = 17

б). Напишете функцията closest fs xys, която приема списък от едноместни функции fs
    и списък от двойки xys и връща фукцията от fs, която е на най-малко разстояние от xys.

Примери:
    (closest [(\x -> x ^ 2), (\x -> x + 1)] [(1, 1), (2, 4), (3, 9), (4, 15)]) 5 = 25
-}
-- 1.а.
distance :: (Double -> Double) -> [(Double, Double)] -> Double
distance f xys = sum [abs (f x - y) | (x, y) <- xys]

-- 1.б.
closest :: [(Double -> Double)] -> [(Double, Double)] -> (Double -> Double)
closest [] _ = error "no functions given"
closest [f] _ = f
closest (f:fs) xys = if distance f xys <= distance mf xys then f else mf where
    mf = closest fs xys


{-
Задача 2.
а). Дефинирайте типа Book, който има следните атрибути (компоненти): заглавие, година на 
    издаване и брой продадени копия.
б). Дефинирайте "помощните" функции title, published и sales, който връщат съответно
    заглавието, годината на издаване и броя продадени копия на дадена книга.
в). Дефинирайте фукцията bestsellers books, която приема списък от книги и връща всички 
    бестселъри (книги, който са били продадени в повече от 1 милион екземпляра).
г). Дефинирайте функцията newTitles books year, която приема списък от книги и година, 
    и връща заглавията на тези от тях, които са издадени през дадената година.
-}
data Book = Book String Int Int
    deriving (Read, Show)

title :: Book -> String
title (Book title _ _) = title

published :: Book -> Int
published (Book _ year _) = year

sales :: Book -> Int
sales (Book _ _ sales) = sales

bestsellers :: [Book] -> [Book]
bestsellers books = [b | b <- books, sales b > 1000000]

newTitles :: [Book] -> Int -> [String]
newTitles books year = [title b | b <- books, published b == year]


{-
Задача 3. Дефинирайте функциата treePaths tree, която връща всички пътища
от корена на дървото до листата му.
-}
treePaths :: Tree a -> [[a]]
treePaths Empty = []
treePaths (Node x Empty Empty) = [[x]]
treePaths (Node x left right) = [x:p | p <- treePaths left ++ treePaths right]


{-
Задача 4. Нека е дадено двоично дърво tree. Дефинирайте функцията 
findMeanNodes tree, която връща списък с всички нодове на tree, чиято 
стойност е равна на средното аритметично на родителя (ако има такъв)
и децата на дадения нод.
-}
findMeanNodes :: (Eq a, Fractional a) => Tree a -> [a]
findMeanNodes tree = helper tree (children tree) where
    helper Empty _  = []
    helper _ [] = []
    helper (Node v left right) nbhs =
        if v == avg nbhs 
            then v : meanChildren 
            else meanChildren where
                meanChildren = helper left (v:children left) ++ helper right (v:children right)

-- помощни ф-и:
avg :: (Fractional a) => [a] -> a
avg xs = sum xs / fromIntegral (length xs)

children :: Tree a -> [a]
children tree = nodesAtLevel tree 1


{-
Задача 5. Да се дефинира функция findGrandpas tree, която за дадено двоично дърво от
естествени числа tree намира списък от всички числа - върхове на tree, които са равни на 
сумата от внуците си. Напишете и примерни извиквания на дефинираната функция.
-}
findGrandpas :: (Eq a, Num a) => Tree a -> [a]
findGrandpas Empty = []
findGrandpas tree@(Node x left right) = if x == sum grandchildren
    then x : xs else xs where
    grandchildren = nodesAtLevel tree 2
    xs = findGrandpas left ++ findGrandpas right


-- примерни извиквания --
main :: IO()
main = 
  let 
    books = [
          Book "Gone with the Wind" 1936 35000000,
          Book "To Kill a Mockingbird" 1960 32000000,
          Book "Chocolate" 1999 995000]
    b = head books
    {- 
    В примерите ще използваме следното дърво (tree): 

                                  3
                                /   \
                               1     4
                              / \   / \
                             0   2     5
                            / \ / \   / \
                                         6
    -}
    tree = (Node 3 (Node 1 (Node 0 Empty Empty) (Node 2 Empty Empty)) (Node 4 Empty (Node 5 Empty (Node 6 Empty Empty))))
  in do
    -- Задача 1.
    print $ distance (\x -> x ^ 2) [(1, 1), (2, 4), (3, 9), (4, 15)]
    print $ distance (\x -> x + 1) [(1, 1), (2, 4), (3, 9), (4, 15)]
    print $ closest [(\x -> x ^ 2), (\x -> x + 1)] [(1, 1), (2, 4), (3, 9), (4, 15)] 5

    -- Задача 2.
    print $ b
    print $ title $ b
    print $ published $ b
    print $ sales $ b
    print $ bestsellers books
    print $ newTitles books 1999

    -- Задача 3.
    print $ treePaths tree

    -- Задача 4.
    print $ findMeanNodes Empty
    print $ findMeanNodes tree
    print $ findMeanNodes (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))

    -- Задача 5.
    print $ findGrandpas Empty
    print $ findGrandpas tree


