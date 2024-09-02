-- EJERCICIO 1

max2 :: Ord a => (a, a) -> a
max2 (x,y)  | x >= y = x
            | otherwise = y

max2Curryfied :: Ord a => a -> a -> a 
max2Curryfied x y   | x >= y = x
                    | otherwise = y

normaVectorial :: (Float, Float) -> Float
normaVectorial (x,y) = sqrt (x^2 + y^2)

normaVectorialCurrified :: Float -> Float -> Float
normaVectorialCurrified x y = sqrt (x^2 + y^2)

substract :: Float -> Float -> Float
substract = flip (-)

predecesor :: Float -> Float
predecesor = substract 1

evaluarEnCero :: (Int -> b) -> b
evaluarEnCero = \f -> f 0

dosVeces :: (a -> a) -> (a -> a)
dosVeces = \f -> f . f

flipAll :: [a -> b -> c] -> [b -> a -> c]
flipAll = map flip

flipRaro :: b -> (a -> b -> c) -> a -> c
flipRaro = flip flip

-- EJERCICIO 2

curriar :: ((a, b) -> c) -> (a -> b -> c)
curriar f = \x y -> f (x, y)

descurriar :: (a -> b -> c) -> ((a,b) -> c)
descurriar f = \(x,y) -> f x y

-- Diria que no puede ser curryN porque nos sabemos a priori el largo de la tupla, y por ende, el tipo

-- Ejercicio 3

-- I
sumR :: Num a => [a] -> a
sumR = foldr (+) 0 

elemR :: Eq a => a -> [a] -> Bool
elemR = \ x l -> foldr (\y rec -> y == x || rec) False l

ppR :: [a] -> [a] -> [a]
ppR = \ l1 l2 -> foldr (:) l2 l1

filterR :: (a -> Bool) -> [a] -> [a]
filterR p = foldr (\x rec -> if p x then x : rec else rec) []

mapR :: (a -> b) -> [a] -> [b]
mapR f = foldr ((:) . f) []

-- II
mejorSegun :: (a -> a -> Bool) -> [a] -> a
mejorSegun f = foldr1 (\x rec -> if f x rec then x else rec)

-- III
sumasParciales :: Num a => [a] -> [a]
sumasParciales = foldr (\x rec -> (:) x (map (+x) rec)) []

-- IV
sumAlt :: Num a => [a] -> a
sumAlt = foldr (\x rec -> x - rec) 0

-- V
sumAltReves :: Num a => [a] -> a
sumAltReves = foldl (\acc x -> x - acc) 0

-- Ejercicio 4
permutaciones :: [a] -> [[a]]
permutaciones = foldr (\x rec -> concatMap (insertInEachPosition x) rec) [[]]

insertInEachPosition :: a -> [a] -> [[a]]
insertInEachPosition x xs = insertInEachPositionFrom 0 x xs

insertInEachPositionFrom :: Int -> a -> [a] -> [[a]]
insertInEachPositionFrom i x xs
    | i > length xs = []
    | i < 0         = insertInEachPositionFrom 0 x xs
    | otherwise     = (take i xs ++ [x] ++ drop i xs) : insertInEachPositionFrom (i+1) x xs

partes :: [a] -> [[a]]
partes = foldr (\x rec -> (concatMap ((: []) . (x:)) rec) ++ rec) [[]]

prefijos :: [a] -> [[a]]
prefijos = foldl (\acc x -> (mejorSegunLongitd acc ++ [x]) : acc) [[]]

masLargaQue :: [a] -> [a] -> Bool
masLargaQue = \x y -> length x > length y

mejorSegunLongitd :: [[a]] -> [a]
mejorSegunLongitd = mejorSegun masLargaQue

sublistas :: [a] -> [[a]]
sublistas = recr (\x xs rec -> prefijosSinVacio (x:xs) ++ rec) [[]]

recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr f z []     = z
recr f z (x:xs) = f x xs (recr f z xs)

prefijosSinVacio :: [a] -> [[a]]
prefijosSinVacio x = foldl (\acc x -> (mejorSegunLongitd acc ++ [x]) : acc) [[head x]] (tail x)

-- Ejercicio 5

-- elementosEnPosicionesPares no es recursion estructural, sino primitiva. Esto es porque utiliza xs por fuera de la expresion elementosEnPosicionesPares xs

-- entrelazar es recursion estructural!

entrelazar :: [a] -> [a] -> [a]
entrelazar = foldr f id

f :: c -> ([c] -> [c]) -> [c] -> [c]
f x rec ys = if null ys 
                then x : rec []
                else x : head ys : rec (tail ys)

-- EJERCICIO 6

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna x = recr (\y ys rec -> if x == y
                                then ys 
                                else  y : rec)
                []

-- foldr no es adecuado porque necesito usar ys por fuera de sacarUna ys

insertarOrdenado :: Ord a => a -> [a] -> [a]
insertarOrdenado x = recr (\y ys rec -> if x <= y 
                                        then x : y : ys
                                        else y : rec) [x]

-- Ejercicio 7
genLista :: a -> (a -> a) -> Int -> [a]
genLista _ _ 0 = []
genLista x f n = x : genLista (f x) f (n-1)

desdeHasta :: Int -> Int -> [Int]
desdeHasta i j = genLista i (\x -> x+1) (j-i+1)

-- Ejercicio 8
mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = foldr (\x rec -> (uncurry f x) : rec) []

armarPares :: [a] -> [b] -> [(a,b)]
armarPares = foldr  (\x rec ys ->
                                if null ys
                                then rec []
                                else (x, head ys) : rec (tail ys)) 
                    (const [])

mapDoble :: (a -> b -> c) -> [a] -> [b] -> [c]
mapDoble f = foldr (\x rec ys -> f x (head ys) : rec (tail ys)) (const [])

-- Ejercicio 9

sumaMat :: [[Int]] -> [[Int]] -> [[Int]]
sumaMat = zipWith (\x y -> zipWith (\z1 z2 -> z1 + z2) x y)

-- transponer :: [[Int]] -> [[Int]]

-- EJERCICIO 10

foldNat :: (Integer -> b -> b) -> b -> Integer -> b
foldNat f z 0 = z
foldNat f z n = f n (foldNat f z (n-1))

potencia :: Integer -> Integer -> Integer
potencia n = foldNat (\x rec -> n * rec) 1

-- Ejercicio 11
data Polinomio a = X
                | Cte a
                | Suma (Polinomio a) (Polinomio a)
                | Prod (Polinomio a) (Polinomio a)

foldPol :: b -> (a -> b) -> (b -> b -> b) -> (b -> b -> b) -> Polinomio a -> b
foldPol cX cCte cSuma cProd X = cX
foldPol cX cCte cSuma cProd (Cte c) = cCte c
foldPol cX cCte cSuma cProd (Suma p1 p2) = 
    cSuma (foldPol cX cCte cSuma cProd p1) (foldPol cX cCte cSuma cProd p2)
foldPol cX cCte cSuma cProd (Prod p1 p2) = 
    cProd (foldPol cX cCte cSuma cProd p1) (foldPol cX cCte cSuma cProd p2)

evaluar :: Num a => a -> Polinomio a -> a
evaluar n = foldPol n id (+) (*)

-- Ejercicio 12

data AB a = Nil | Bin (AB a) a (AB a)

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB cNil cBin Nil = cNil
foldAB cNil cBin (Bin i r d) = 
    cBin (foldAB cNil cBin i) r (foldAB cNil cBin d)

recAB :: b -> (b -> AB a -> a -> AB a -> b -> b) -> AB a -> b
recAB cNil cBin Nil = cNil
recAB cNil cBin (Bin i r d) = 
    cBin (recAB cNil cBin i) i r d (recAB cNil cBin d)


esNil :: AB a -> Bool
esNil a = case a of
            Nil -> True
            Bin l r d -> False 

altura :: AB a -> Int
altura = foldAB 1 (\recL r recD -> 1 + (max recL recD))

cantNodos :: AB a -> Int
cantNodos = foldAB 1 (\recL r recD -> recL + 1 + recD)

-- foldAB1 :: (b -> a -> b -> b) -> AB a -> b
-- foldAb1 cBin (Bin Nil r Nil) = r
-- foldAb1 cBin (Bin _ r Nil) = cBin (foldAB1 cBin i) r (foldAB1 cBin d)
-- foldAb1 cBin (Bin Nil r _) = cBin (foldAB1 cBin i) r (foldAB1 cBin d)
-- foldAB1 cBin (Bin i r d) = 
--     cBin (foldAB1 cBin i) r (foldAB1 cBin d)

-- mejorSegunAB :: (a -> a -> Bool) -> AB a -> a
-- mejorSegunAB f = foldAB1 (\recL r recD -> mejorEntreTresSegun f recL r recD)

-- mejorEntreTresSegun :: (a -> a -> Bool) -> a -> a -> a -> a
-- mejorEntreTresSegun f x y z = if f x y
--                                 then if f x z then x else z
--                                 else if f y z then y else z
-- Esto probablemente se haga mirando que el subarbol no sea nil o algo

esABB :: Ord a => AB a -> Bool
esABB = recAB True (\recL l r d recD -> recL && recD && esBinB l r d)

esBinB :: Ord a => AB a -> a -> AB a -> Bool
esBinB Nil _ Nil = True
esBinB (Bin l rl d) r Nil = r > rl
esBinB Nil r (Bin l rd d) = r < rd
esBinB (Bin ll rl dl) r (Bin ld rd dd) = r > rl && (r < rd)

-- Ejercicio 13
-- ramas :: AB a -> Int
-- No se la verdad que es lo que quieren que haga aca

cantHojas :: AB a -> Int
cantHojas = foldAB 1 (\recL r recD -> recL + recD)

espejo :: AB a -> AB a
espejo = foldAB Nil (\recL r recD -> Bin recD r recL)

mismaEstructura :: AB a -> AB b -> Bool
mismaEstructura = foldAB esNil (\recL r recD a ->
        (not (esNil a)) && (recL (goLeft a)) && (recD (goRight a)))

goLeft :: AB a -> AB a
goLeft (Bin l r d) = l

goRight :: AB a -> AB a
goRight (Bin l r d) = d

-- Ejercicio 14
data AIH a = Hoja a | BinAIH (AIH a) (AIH a)

foldAIH :: (a -> b) -> (b -> b -> b) -> AIH a -> b
foldAIH cHoja cBin (Hoja a) = cHoja a
foldAIH cHoja cBin (BinAIH l r) = 
    cBin (foldAIH cHoja cBin l) (foldAIH cHoja cBin r)

alturaAIH :: AIH a -> Integer
alturaAIH = foldAIH (const 1) (\recL recD -> (max recL recD) + 1)

sizeAIH :: AIH a -> Integer
sizeAIH = foldAIH (const 1) (\recL recD -> recL + recD)

-- Ejercicio 15

data RoseTree a = HojaRT a | BinRT [RoseTree a]
-- Le agrego valor porque despues las tengo que mostrar a las hojas

foldRT :: (a -> b) -> ([b] -> b) -> RoseTree a-> b
foldRT cHoja cBin (HojaRT r)    = cHoja r
foldRT cHoja cBin (BinRT h)     = cBin (map (foldRT cHoja cBin) h) 

hojasRT :: RoseTree a -> [a]
hojasRT = foldRT (\x -> [x]) (\h -> concat h) 

distanciasRT :: RoseTree a -> [Int]
distanciasRT = foldRT (const [0]) (\h -> map (+1) (concat h))

alturaRT :: RoseTree a -> Int
alturaRT = foldRT (const 1) (\h -> (mejorSegun (>) h) + 1)
-- c) altura, que devuelve la altura de un RoseTree (la cantidad de nodos de la rama más larga). Si el
-- RoseTree es una hoja, se considera que su altura es 1.