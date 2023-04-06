{- Асоциативни списъци и графи. Представяне и пътища в граф. -}
import Data.List

{-
Задача 1. Нека е даден списък от двойки edges, съответстващ на ребрата на даден
ориентиран граф. Дефинирайте следните функции:

а). nodes edges, която връща списъкс с нодовете на съответния граф.
б). neighbours edges node, която връща списък със съседите на даден връх.
в). adjacencyList edges, която връща списъка на наследниците на съответния граф.

Примери:
    nodes [(1, 2), (1, 3), (2, 3), (2, 4)] -> [1, 2, 3, 4]

    neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2 -> [3, 4]
    neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 4 -> []

    adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)] -> [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])]
-}
nodes :: (Eq a) => [(a, a)] -> [a]
nodes edges = nub $ map fst edges ++ map snd edges

nodes' edges = nub $ xs ++ ys where
    (xs, ys) = unzip edges

neighbours :: (Eq a) => [(a, a)] -> a -> [a]
neighbours edges node = [to | (from, to) <- edges, from == node]

neighbours' edges node = map snd $ filter ((node ==) . fst) edges

adjacencyList :: (Eq a) => [(a, a)] -> [(a, [a])]
adjacencyList edges = [(node, neighbours edges node) | node <- nodes edges]

adjacencyList' edges = zip ns $ map (neighbours edges) ns where
    ns = nodes edges


{-
Задача 2. Дефинирайте функцията isPath adjs nodes, която приема списък на наследниците
на даден граф adjs и списък от върхове nodes и връща дали nodes е път в съответния граф.

Примери:
    isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4] -> True
    isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4] -> False
-}
findAssoc :: (Eq a) => [(a, b)] -> a -> b
findAssoc [] _ = error "not found!"
findAssoc ((k, v):xs) key
    | k == key  = v
    | otherwise = findAssoc xs key

findAssoc' xs key = head [v | (k, v) <- xs, k == key]

isPath :: (Eq a) => [(a, [a])] -> [a] -> Bool
isPath adjs nodes = and [to `elem` findAssoc adjs from | (from, to) <- zip nodes (drop 1 nodes)]

isPath' adjs (n1:n2:ns) = n2 `elem` findAssoc adjs n1 && isPath adjs (n2:ns)
isPath' adjs _ = True


{-
Задача 3. Нека е дадено двойчно дърво, представено като списък от тройки,
чиито първи елемент е идентификатор на дадения връх, а 2-рият и 3-тият,
идентификаторите на съответно лявото и дясно дете на върха.

Дефинирайте функцията listLeaves nodes, която връща списък с всички листа
на даденото дърво.

Примери:
    listLeaves [(1, 2, 3), (2, 4, 5)] -> [4, 3, 5]
    listLeaves [(2, 4, 5), (1, 2, 3)] -> [4, 5, 3]
-}
listLeaves :: (Eq a) => [(a, a, a)] -> [a]
listLeaves nodes = filterLeaves getLeftChild ++ filterLeaves getRigthChild where
    getNodeId (nodeId, _, _) = nodeId
    getLeftChild (_, leftChild, _) = leftChild
    getRigthChild (_, _, rigthChild) = rigthChild
    nodeIds = map getNodeId nodes
    filterLeaves getId = filter (\id -> not $ id `elem` nodeIds) $ map getId nodes


{-
Задача 4*. Дефинирайте функцията simplePaths adjs k node, която приема списък на
наследниците adjs на даден ориентиран граф, цяло число k и идентификатор на връх
node и връща всички прости пътища с дължина k, които започват от node.

Примери:
    simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1 -> [[1]]
    simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1 -> [[1, 2], [1, 3]]
    simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1 -> [[1, 2, 3], [1, 2, 4]]
-}
simplePaths :: (Eq a) => [(a, [a])] -> Int -> a -> [[a]]
simplePaths adjs k node
    | k < 0     = error "k >= 0"
    | k == 0    = [[node]]
    | otherwise = map (node:) $ filter (not . (node `elem`)) $ concat [simplePaths adjs (k - 1) n | n <- findAssoc adjs node]


-- ТЕСТОВЕ НА ЗАДАЧИТЕ --
main = do
    print $ nodes [(1, 2), (1, 3), (2, 3), (2, 4)]
    print $ nodes' [(1, 2), (1, 3), (2, 3), (2, 4)]
    print $ neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 2
    print $ neighbours [(1, 2), (1, 3), (2, 3), (2, 4)] 4
    print $ neighbours' [(1, 2), (1, 3), (2, 3), (2, 4)] 2
    print $ neighbours' [(1, 2), (1, 3), (2, 3), (2, 4)] 4
    print $ adjacencyList [(1, 2), (1, 3), (2, 3), (2, 4)]
    print $ adjacencyList' [(1, 2), (1, 3), (2, 3), (2, 4)]

    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4]
    print $ isPath [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4]
    print $ isPath' [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 2, 4]
    print $ isPath' [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] [1, 3, 4]

    print $ listLeaves [(1, 2, 3), (2, 4, 5)]
    print $ listLeaves [(2, 4, 5), (1, 2, 3)]

    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 0 1
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 1 1
    print $ simplePaths [(1, [2, 3]), (2, [3, 4]), (3, []), (4, [])] 2 1
    print $ simplePaths [(1, [2]), (2, [1, 3]), (3, [])] 2 1
