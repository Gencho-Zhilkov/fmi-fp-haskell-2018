import Prelude hiding ((.))
{-
Ламбда изрази, частично прилагане на функции, композиция, оператор ($)

I. Ламбда изрази: в Haskell всички ф-и са ламбда изрази.
    
В Haskell ламбда изразите изглеждат така:

    foo :: Int -> Int 
    foo = \x -> 2 * x

което е АБСОЛЮТНО съшото като:

    foo :: Int -> Int 
    foo x = 2 * x

Още примери (Групираните по-долу дефиниции са еквивалиентни)

    bar :: Int -> Int -> Int
    bar = \x -> \y -> x + y
    bar = \x y -> x + y
    bar x y = x + y

    baz :: Int -> Int -> Int -> Int
    baz =   \x ->  \y ->  \z -> x + y * z
    baz = \x y z -> x + y * z
    baz = \x -> \y -> \z -> x + y * z

II. Частично прилагане на функции

    Частично прилагане на функции означава да подадем по-малък брой аргументи от броя на параметри
    на функцията и да получим нова функция на останалия брой параметри.

    mult :: Integer -> Integer -> Integer
    mult x y = x * y
    mult 5   ->   резултата е функция от вида (mult 5) :: Integer -> Integer, т.е. единия параметър
                  вече е фиксиран

    За какво може да ни послужи това?
    Ако искаме да умножим всички числа в един списък по числото две, не е нужно да правим нов ламбда израз:
    map (\x -> x ^ 2) [1, 2, 3]   ->   [1, 4, 6]
    map (mult 2) [1, 2, 3]        ->   [1, 4, 6]
    
    Имаме функцията:
    pow :: Integer -> Integer -> Integer
    pow x y = x ^ y

    Искаме да повдигнем всички елементи в списък на квадрат. Ще сработи ли следният код?
    map (pow 2) [1, 2, 3]   ->   [1, 4, 8]

    В този случай няма да получим [1, 4, 9], както искаме, а ще получим [1, 4, 8], тъй като
    от дефиницията на pow повдигаме първия параметър на степен втория параметър.
    Затова Haskell ни предоставя следният синтаксис който важи само за функции от 2 параметъра с инфиксен запис.

    map (`pow` 2) [1, 2, 3]   ->   [1, 4, 9]

    По този начин можем да прилагаме аргументи отляво и отдясно на функцията. Ето малко други примери:

    map (2^) [1, 2, 3]   ->   [1, 4, 8]
    map (^2) [1, 2, 3]   ->   [1, 4, 9]
    map (+1) [1, 2, 3]   ->   [2, 3, 4]
    map (1+) [1, 2, 3]   ->   [2, 3, 4]

    Ето още един интересен случай:
    map (`elem` [1, 2, 3]) [2, 5, 3]  ->  [True, False, True]


III. Композиция на функции и оператор ($)

    1. Композиция на функции.
    Дефниция: Ако имаме две функции f и g, то тяхна композиция наричаме фунцкията (f . g)(x) = f(g(x)).
              "(f . g)" се произнася f след g.
    В Haskell, както видяхте, за композиране се използва операторът (.) :: (b -> c) -> (a -> b) -> (a -> c)

Примери:
    f = (^2)
    g = (\x -> x * 2)

    (f . g) 2 -> (f . g)(2) -> f(g(2)) -> f(2*2) -> f(4) -> 4^2 -> 16
    Забележете, че изразът (f . g) връща функция, след което върху тази получена функция прилагаме аргумента 2.

    2. Оператор ($)
    Инфикцият оператор ($) се използва за премахване на скоби от кода. Той оценява напълно израза отдясно преди
    да го подаде на функцията отляво.

Примери:

    abs (3 - 12) -> 9
    abs $ 3 - 12 -> 9

Примери за използване на двата оператора заедно:
    f = (-5)
    g = (*2)
    h = (+8)

    (f (g (h (4 + 1))))    -> f (g (h 5)) -> f (g (5 + 8)) -> f (13 * 2) -> (26 - 5) -> 21
    f . g . h $ 4 + 1      -> f (g (h 5)) -> f (g (5 + 8)) -> f (13 * 2) -> (26 - 5) -> 21
-}

{-
Задача 1. Дефинирайте инфиксния оператор (.) за композиция на функции. (Премахнат е от Prelude,
за да можем да си го дефинираме сами).

Пример:
    (^2) . (+1) $ 5   -> (5+1)^2 -> 36
    (+1) . (+1) $ 2   -> 4
    Извикваме оператора (.) върху две едноаргументни функции и получаваме като РЕЗУЛТАТ ТРЕТА ФУНКЦИЯ
    която е тяхната композиция. Върху този резултат от f . g, който е ФУНКЦИЯ, прилагаме
    някакъв аргумент x за да получим стойността на композицията в тази стойност на x.
-}
(.) :: (b -> c) -> (a -> b) -> (a -> c)
f . g = undefined


{- 
Задача 2. Дефинирайте следните функции от по-висок ред:
а). fmin f g, която приема две едноместни числови функции f и gh
и връща едноместни числова функция, чиято стойност в точка x е
минимума на f и g.
б). fmax f g, като fmin, но връща максимума на f и g.
в). favg f g, като fmin, но връща средното аритметично на f и g.

f = (**2)
f = (\x -> x ** 2)
f x = x ** 2

g = (2**)
g = (\x -> 2 ** x)
g x = 2 ** x

Примери:
    Тук долара може да се изпусне но е важно да се запомни, че резултата
    на fmin f g е функция върху която прилагаме "3-тия" аргумент.
    (fmin f g) 3    -> 8
    fmin f g $ 5    -> 25
    fmax f g $ 3    -> 9
    fmax f g 5      -> 32
    favg f g 3      -> 8.5
    favg f g 5      -> (25 + 32) / 2 -> 28.5
-}
fmin :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
fmin f g = undefined

fmax :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
fmax f g = undefined

favg :: (Double -> Double) -> (Double -> Double) -> (Double -> Double)
favg f g = undefined


{-
Задача 3. Дефинирайте следните функции от по-висок ред:
а). boundUp f up, която приема едноместни числова функция f и
и числова стойност up и връща едноместни числова функция, чиято
стойност в точка x е минимума на f(x) и up.
б). boundDown f down, същата като boundUp, но връща максимума
на f(x) и down.

Примери:
    f = \x -> x + 1

    boundUp f 5 $ 1              -> 2
    boundUp f 5 $ 1              -> 2
    boundUp (\x -> x + 1) 5 $ 5  -> 5
    boundUp (^3) 25 $ 2          -> 8 
    boundUp (^3) 15 $ 3          -> 15 

    boundDown (^2) 5 $ 1         -> 5
    boundDown (^2) 5 $ 3         -> 9
-}
boundUp :: (Double -> Double) -> Double -> (Double -> Double)
boundUp f up = undefined

boundDown :: (Double -> Double) -> Double -> (Double -> Double)
boundDown f down = undefined


{-
Задача 4. Производна на функция: Напишете функцията derivative f dx, която приема едноместна 
числова функция f и стъпка dx и връща функция, чиято стойност в дадена точка x, е приближението 
на производната на f в x.

Примери:
    f' = (\x -> x ^ 3 / 3) `derivative` 1e-6

    f' 0 = 3.333333333333333e-13
    f' 1 = 1.000000000001
    f' 2 = 4.000000000115023
    f' 10 = 99.99999994647624
-}
derivative :: (Double -> Double) -> Double -> (Double -> Double)
derivative f dx = undefined


{-
Задача 5. Напишете функцията maximize, която получава непразен списък от едноместни
числови функции и връща нова едноместна числова функция на аргумент x, която дава 
стойността f x на тази фунция f от списъка, за която числото f x е най-голямо 
по абсолютна стойност.

Пример: 
    maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5  = 1.5
    maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2) = 16
-}
maximize :: [Double -> Double] -> (Double -> Double)
maximize fs = undefined


{-
Задача 6. Напишете функцията compose fs, която приема списък от едноаргументни функцуии
и връща тяхната композиция, т.е. compose [f1, f2, .. fn] x = f1(f2( ... (fn(x))))

Пример: 
    compose [(+1), (2*)] 7 = (2 * 7) + 1 = 15
    compose [(+1), (+1), (+1)] 7 = 10
-}
compose :: [(a -> a)] -> (a -> a)
compose fs = undefined


{-
Задача 7. Дефинирайте функцията closestPoint xys), която приема
списък от точки в равнината (представени чрез двойки (x . y)) и връща
едноаргументна функция, чиято стойност в дадена точка p e най-близката
до p точка от xys.

Примери:
    closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (3, 3)    -> (0, 0)
    closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (6, 6)    -> (10, 10)
    closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (10, 1)   -> (10, 0)
    closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (1, 10)   -> (0, 10)

-}
type Point2D = (Double, Double)

closestPoint :: [Point2D] -> (Point2D -> Point2D)
closestPoint xys = undefined


-- ТЕСТОВЕ НА ЗАДАЧИТЕ --
main = do
    print "Exercise 1."
    print $ (^2) . (+1) $ 5
    print $ (+1) . (+1) $ 2

    print "Exercise 2."
    print $ (fmin (**2) (2**)) 3
    print $ fmin (**2) (2**) $ 5
    print $ fmax (**2) (2**) $ 3
    print $ fmax (**2) (2**) 5  
    print $ favg (**2) (2**) 3  
    print $ favg (**2) (2**) 5  

    print "Exercise 3."
    print $ boundUp (+1) 5 $ 1
    print $ boundUp (+1) 5 $ 1
    print $ boundUp (+1) 5 $ 5
    print $ boundUp (^3) 25 $ 2
    print $ boundUp (^3) 15 $ 3
    print $ boundDown (^2) 5 $ 1
    print $ boundDown (^2) 5 $ 3

    print "Exercise 4."
    print $ (\x -> x ^ 3 / 3) `derivative` 1e-6 $ 0
    print $ (\x -> x ^ 3 / 3) `derivative` 1e-6 $ 1
    print $ (\x -> x ^ 3 / 3) `derivative` 1e-6 $ 2
    print $ (\x -> x ^ 3 / 3) `derivative` 1e-6 $ 10

    print "Exercise 5."
    print $ maximize [(\x -> x ** 3), (\x -> x + 1)] 0.5
    print $ maximize [(\x -> x ** 3), (\x -> x + 1), (\x -> x ** 4)] (-2)

    print "Exercise 6."
    print $ compose [(+1), (2*)] 7
    print $ compose [(+1), (+1), (+1)] 7

    print "Exercise 7."
    print $ closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (3, 3)
    print $ closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (6, 6)
    print $ closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (10, 1)
    print $ closestPoint [(0, 0), (0, 10), (10, 0), (10, 10)] $ (1, 10)
