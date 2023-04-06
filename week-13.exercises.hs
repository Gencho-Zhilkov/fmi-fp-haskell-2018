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
distance f xys = undefined

-- 1.б.
closest :: [(Double -> Double)] -> [(Double, Double)] -> (Double -> Double)
closest fs xys = undefined


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
type Title = String
type Year = Int
type PublicationYear = Year
type CopiesSold = Int

data Book = Book Title PublicationYear CopiesSold
    deriving (Read, Show)

title :: Book -> Title
title book = undefined

published :: Book -> PublicationYear
published book = undefined

sales :: Book -> CopiesSold
sales book = undefined

bestsellers :: [Book] -> [Book]
bestsellers books = undefined

newTitles :: [Book] -> Year -> [Title]
newTitles books year = undefined


{-
Задача 3. Дефинирайте функциата treePaths tree, която връща всички пътища
от корена на дървото до листата му.
-}
treePaths :: Tree a -> [[a]]
treePaths tree = undefined


{-
Задача 4. Нека е дадено двоично дърво tree. Дефинирайте функцията 
findMeanNodes tree, която връща списък с всички нодове на tree, чиято 
стойност е равна на средното аритметично на родителя (ако има такъв)
и децата на дадения нод.
-}
findMeanNodes :: (Eq a, Fractional a) => Tree a -> [a]
findMeanNodes = undefined


{-
Задача 5. Да се дефинира функция findGrandpas tree, която за дадено двоично дърво от
естествени числа tree намира списък от всички числа - върхове на tree, които са равни на 
сумата от внуците си. Напишете и примерни извиквания на дефинираната функция.
-}
findGrandpas :: (Eq a, Num a) => Tree a -> [a]
findGrandpas tree = undefined


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
