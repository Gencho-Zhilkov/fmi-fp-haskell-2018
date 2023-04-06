import Data.List 


{-
Задача 1. Нека f е функция от тип Int -> Int, а lst е списък от цели числа [a1,a2, … ,an].
Дефинирайте функция bind f lst, която връща функция от тип Int -> Int, чиято стойност за
дадено цяло число n е равна на f(x), Където x e най-голямото от числата от lst, чиято
позиция в lst е кратна на n.
-}
bind :: (Int -> Int) -> [Int] -> (Int -> Int)
bind f lst = undefined


-- Задача 2. Нека е дефиниран типа Tree, описващ дърво, чиито стойности са цели числа 
data Tree = Empty | Node Int Tree Tree
    deriving (Read, Show)

{-
Дефинирайте функцията heavyNodes tree, която връща списък със стойностите на всички върхове
на дървото tree, които са по-големи от сбора на предшествениците си.
-}
heavyNodes :: Tree -> [Int]
heavyNodes tree = undefined


-- Задача 3. Нека са дефинирани следните типове:
type Name = String      -- име на отбор и играч 
type Goals = Int        -- брой отбелязани попадения
type Assists = Int      -- брой асистенции
type Hometown = Name    -- име на града на даден отбор

-- описание да играч
data Player = Player Name Goals Assists
    deriving (Read, Show)

-- описание на отбор
data Team = Team Name Hometown [Player]
    deriving (Read, Show)

{-
Дефинирайте следните функции:

а). topScorrer, която приема списък с отбори и връща името на играча с най-много попадения.
б). topTeam, която приема списък с отбори и връща името на отбора с най-много попадения.
в). topAssists, която приема списък с отбори и връща името на играча с най-много асистенции.
г). topCity, която приема списък с отбори и връща името на града с най-много отбори в него.
-}

-- а).
topScorrer :: [Team] -> Name
topScorrer teams = undefined

-- б).
topTeam :: [Team] -> Name
topTeam teams = undefined

-- в).
topAssists :: [Team] -> Name
topAssists teams = undefined

-- г).
topCity :: [Team] -> Name
topCity teams = undefined


-- примерни извиквания --
main :: IO()
main = do
    print "Hello world"

