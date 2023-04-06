import Data.List

{- Алгебричин типове -}
{-
Досега сме ползвали наготово различни типове - Bool, Int, Char, [Char], etc.
Как можем да създадем наши типове? С помощта на keyword-а data.
Нека видим как е дефиниран типа Bool:

data Bool = False | True

Това означава, че Bool може да има стойности False или True. False и True наричаме конструктори
на типа. Името на типа и конструкторите задължително започват с главни букви.

По подобен начин можем да приемем, че типа Int е дефиниран по следния начин:

data Int = -2147483648 | -2147483647 | ... | -1 | 0 | 1 | 2 | ... | 2147483647
-}

{- ПРИМЕРИ -}
{- 
Пример 1. Нека е дефиниран алгебричният тип Point2D, описващ точка в 2D пространството.
Да се дефинира функцията distance, която приема две точки и връща разстоянието между тях.

Примери:
    distance (Point2D 1 1) (Point2D 2 2)
-}
data Point2D = Point2D Double Double
    deriving (Read, Show)

distance :: Point2D -> Point2D -> Double
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
distance (Point2D x1 y1) (Point2D x2 y2) = sqrt $ (x1 - x2) ^ 2 + (y1 - y2) ^ 2


{- Дефиниция на полиморфния алгебричен тип List, описващ едносвързан списък (подобно на типа [a]). -}
data List a = Nil | a `Cons` List a
    deriving (Read, Show)

{-
Пример 2. Да се дефинира полиморфната функция  listAppend xs x, която приема списък xs от тип List a
и обект x от тип а и  добавя x в края на xs.   

Примери: 
    listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 10 = 1 `Cons` (2 `Cons` (3 `Cons` (10 `Cons` Nil)))
    listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 42 = 1 `Cons` (2 `Cons` (3 `Cons` (42 `Cons` Nil)))
-}
listAppend :: List a -> a -> List a
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
listAppend Nil x            = x `Cons` Nil
listAppend (h `Cons` ts) x  = h `Cons` (listAppend ts x)


{- Дефиниция на типа Tree a описващ произволно двоично дърво. -}
data Tree a = Empty | Node a (Tree a) (Tree a)
    deriving (Read, Show)

{-
Пример 3. Да се дефинира функцията treeEmpty tree, която връща дали двоичното
дърво tree е празно.
-}
treeEmpty :: Tree a -> Bool
-- използваме съпоставяне по шаблон, с по един шаблон за всеки от случайте
treeEmpty Empty = True
treeEmpty _ = False

{-
Пример 4. Да се дефинира функцията treeRoot tree, която връща корена на
двоичното дърво tree.
-}
treeRoot :: Tree a -> a
treeRoot Empty = error "empty tree!"
treeRoot (Node val _ _) = val

{-
Пример 5. Да се дефинира функцията treeCount tree, която връща броя на
елементите на двоичното дърво tree.
-}
treeCount :: (Num b) => Tree a -> b
treeCount Empty = 0
treeCount (Node _ left right) = 1 + treeCount left + treeCount right


{- ЗАДАЧИ -}
{- Задача 1. Нека са дефинирани типовете: -}
type Student = String  -- име на ученик
type Subject = String  -- име на предмет
type Note = Double     -- оценка

-- Запис с име на ученик, предмет и оценката на ученика по дадения предмет.
data Record = Record Student Subject Note
    deriving (Read, Show)

{-
Дефинирайте функцията goodStudentsAverage:: [Record] -> Note, която връща
средната стойност от оценките на всички ученици, които имат поне една шестица.
-}
goodStudentsAverage:: [Record] -> Note
goodStudentsAverage records = undefined


{-
Задача 2. Нека е даден полиморфният алгебричен тип List (дефиниран в пример 1). 
Дефинирайте (рекурсивно) следните функции:
а). mkList, която приема стандартен списък и го превръща в списък от тип List.
б). unList, която приема списък от тип List и го превръща в стандартен списък.
в). listEmpty, същата като empty, но за списък от тип List.
г). listHead, същата като head, но за списък от тип List.
д). listTail, същата като tail, но за списък от тип List.
е). listMap, същата като map, но за списък от тип List.
ж). listFilter, същата като filter, но за списък от тип List.

Примери:
    mkList [1, 2, 3] = 1 `Cons` (2 `Cons` (3 `Cons` Nil))
    unList (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = [1, 2, 3]
    listEmpty (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = False
    listHead (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = 1
    listTail (1 `Cons` (2 `Cons` (3 `Cons` Nil)))  = 2 `Cons` (3 `Cons` Nil)
    listMap (+1) (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = 2 `Cons` (3 `Cons` (4 `Cons` Nil))
    listFilter even (1 `Cons` (2 `Cons` (3 `Cons` Nil))) = 2 `Cons` Nil
-}
-- 2.а.
mkList :: [a] -> List a
mkList xs = undefined

-- 2.б.
unList :: List a -> [a]
unList xs = undefined

-- 2.в.
listEmpty :: List a -> Bool
listEmpty xs = undefined

-- 2.г.
listHead :: List a -> a
listHead xs = undefined

-- 2.д.
listTail :: List a -> List a
listTail xs = undefined

-- 2.e.
listMap :: (a -> a) -> List a -> List a
listMap f xs = undefined

-- 2.ж.
listFilter :: (a -> Bool) -> List a -> List a
listFilter f xs = undefined


{- Двоични дървета -}
{-
Задача 3. Нека е дадено двоично дърво tree. Дефинирайте следните функции:

а). treeDepth tree, която връща дълбочината на дървото.
б). treeCountLeaves tree, която връща броя на листата на дървото.
в). treeSum tree, която връща сбора на всички стойности в дървото.
г). treeElem val tree, която проверява дали дадена стойност val е в дървото.
д). treeNodes tree, която връща списък със стойностите в дървото.
е). treeNodesAtLevel tree n, която връща списък със стойностите в n-тото ниво на дървото.
-}
treeDepth :: (Num b, Ord b) => Tree a -> b
treeDepth tree = undefined

treeCountLeaves :: (Num b) => Tree a -> b
treeCountLeaves tree = undefined

treeSum :: Num a => Tree a -> a
treeSum tree = undefined

treeElem :: Eq a => a -> Tree a -> Bool
treeElem val tree = undefined

treeNodes :: Tree a -> [a]
treeNodes tree = undefined

treeNodesAtLevel :: (Eq b, Num b) => Tree a -> b -> [a]
treeNodesAtLevel tree n = undefined


{-
Задача 4. Нека е дадено двоично дърво tree. Дефинирайте функцията 
findMeanNodes tree, която връща списък с всички нодове на tree, чиято 
стойност е равна на средното аритметично на родителя (ако има такъв)
и децата на дадения нод.
-}
findMeanNodes :: (Eq a, Fractional a) => Tree a -> [a]
findMeanNodes tree = undefined


-- примерни извиквания --
main :: IO()
main = 
  let 
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
    -- Пример 1.
    print $ distance (Point2D 1 1) (Point2D 2 2)

    -- Пример 2.
    print $ listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 10
    print $ listAppend (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 42

    -- Пример 3.
    print $ treeEmpty Empty
    print $ treeEmpty tree
    
    -- Пример 4.
    -- print $ treeRoot Empty
    print $ treeRoot tree
    
    -- Пример 5.
    print $ treeCount Empty
    print $ treeCount tree

    -- Задача 1.
    print $ goodStudentsAverage [
            Record "Alan" "Maths" 5,
            Record "Alan" "Physics" 4.50,
            Record "Alice" "Maths" 4,
            Record "Alice" "Physics" 6,
            Record "Bob" "Maths" 6,
            Record "Bob" "Physics" 5,
            Record "Sally" "Maths" 5.25,
            Record "Sally" "Physics" 4.50]

    -- Задача 2.
    print $ mkList [1, 2, 3]
    print $ unList (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listEmpty (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listHead (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listTail (1 `Cons` (2 `Cons` (3 `Cons` Nil))) 
    print $ listMap (+1) (1 `Cons` (2 `Cons` (3 `Cons` Nil)))
    print $ listFilter even (1 `Cons` (2 `Cons` (3 `Cons` Nil)))

    -- Задача 3.
    print $ treeDepth Empty
    print $ treeDepth tree
    print $ treeCountLeaves Empty
    print $ treeCountLeaves tree
    print $ treeSum Empty
    print $ treeSum tree
    print $ 2 `treeElem` Empty
    print $ 2 `treeElem` tree
    print $ 7 `treeElem` tree
    print $ treeNodes tree
    print $ tree `treeNodesAtLevel` 0
    print $ tree `treeNodesAtLevel` 1
    print $ tree `treeNodesAtLevel` 2
    print $ tree `treeNodesAtLevel` 3
    print $ tree `treeNodesAtLevel` 4

    -- Задача 4.
    print $ findMeanNodes Empty
    print $ findMeanNodes tree
    print $ findMeanNodes (Node 2 (Node 1 Empty Empty) (Node 3 Empty Empty))
