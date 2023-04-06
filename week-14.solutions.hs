import Data.List 


{-
Задача 1. Нека f е функция от тип Int -> Int, а lst е списък от цели числа [a1,a2, … ,an].
Дефинирайте функция bind f lst, която връща функция от тип Int -> Int, чиято стойност за
дадено цяло число n е равна на f(x), Където x e най-голямото от числата от lst, чиято
позиция в lst е кратна на n.
-}
bind :: (Int -> Int) -> [Int] -> (Int -> Int)
bind f lst = \n -> f $ maximum [a | (i, a) <- zip [0 ..] lst, i `mod` n == 0]


-- Задача 2. Нека е дефиниран типа Tree, описващ дърво, чиито стойности са цели числа 
data Tree = Empty | Node Int Tree Tree
    deriving (Read, Show)

{-
Дефинирайте функцията heavyNodes tree, която връща списък със стойностите на всички върхове
на дървото tree, които са по-големи от сбора на предшествениците си.
-}
heavyNodes :: Tree -> [Int]
heavyNodes tree = helper tree 0 where
    helper Empty _ = []
    helper (Node v left right) psum = (if v > psum then (v:) else id) heavyChildren where
        heavyChildren = helper left (v + psum) ++ helper right (v + psum)


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

-- помощни ф-и:
name :: Player -> Name
name (Player name _ _) = name

goals :: Player -> Goals
goals (Player _ goals _) = goals

assists :: Player -> Assists
assists (Player _ _ assists) = assists

argmax :: (Ord b) => (a -> b) -> [a] -> a
argmax f xs = head [x | x <- xs, f x == fmax] where
    fmax = maximum (map f xs)

-- а).
topScorrer :: [Team] -> Name
topScorrer teams = name $ argmax goals (concat [players | (Team _ _ players) <- teams])

-- б).
topTeam :: [Team] -> Name
topTeam teams = fst $ argmax snd [(team, sum $ map goals players) | (Team team _ players) <- teams]

-- в).
topAssists :: [Team] -> Name
topAssists teams = name $ argmax assists (concat [players | (Team _ _ players) <- teams])

-- г).
hometown :: Team -> Hometown
hometown (Team _ hometown _) = hometown

countTeams :: [Team] -> Hometown -> Int
countTeams teams city = length $ filter (\team -> hometown team == city) teams

topCity :: [Team] -> Name
topCity teams = fst $ argmax snd [(city, countTeams teams city) | city <- cities] where
    cities = nub (map hometown teams)


-- примерни извиквания --
main :: IO()
main = do
    print "Hello world"

