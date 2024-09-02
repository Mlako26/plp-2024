-- EJERCICIO 1

-- null :: [a] -> Bool
-- returns True if a list is empty, otherwise False

-- head :: [a] -> a
-- returns the first item of a list

-- tail :: [a] -> [a]
-- it accepts a list and returns the list without its first item

-- init :: 	[a] -> [a]
-- it accepts a list and returns the list without its last item

-- last :: 	[a] -> a
-- returns the last item of a list

-- take ::	Int -> [a] -> [a]
-- creates a list, the first argument determines how many items should be taken from the list passed as the second argument

-- drop :: Int -> [a] -> [a]
-- creates a list, the first argument determines how many items should be dropped from the list passed as the second argument

-- (++) :: [a] -> [a] -> [a]
-- concatenates two lists

-- concat ::[[a]] -> [a]
-- accepts a list of lists and concatenates them

-- reverse :: 	[a] -> [a]
-- creates a new list from the original one with items in the reverse order


-- elem :: a -> [a] -> Bool
--returns True if the list contains an item equal to the first argument

-- concatmap :: (a -> [b]) -> [a] -> [b]
-- creates a list from a list generating function by application of this function on all elements in a list passed as the second argument

-- EJERCICIO 2

valorAbsoluto :: Float ->  Float
valorAbsoluto x | x < 0 = x * (-1)
                | otherwise = x

bisiesto :: Int -> Bool
bisiesto x = (x `mod` 4) == 0

factorial :: Int -> Int
factorial 1 = 1
factorial n = n * (factorial (n-1))

-- cantDesdeHasta :: (Int -> Int -> Bool) -> Int -> Int -> Int -> Int
-- cantDesdeHasta f x y y = if f x y then 1 else 0
-- cantDesde f x y | f x y     = 1 + (cantDesde f x (y+1))
--                 | otherwise = cantDesde f x (y+1)

-- me da paja pero es por ahi para abstraer

-- EJERCICIO 3

inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left x) = x
aEntero (Right p) = if p then 1 else 0

-- EJERCICIO 4

limpiar :: String -> String -> String
limpiar x [] = []
limpiar x (y:ys) 
    | elem y x = limpiar x ys
    | otherwise = y : (limpiar x ys)

promedio :: [Float] -> Float
promedio x = (sum x) / fromIntegral (length x)

difPromedio :: [Float] -> [Float]
difPromedio x = map (+ (negate (promedio x))) x

todosIguales :: [Int] -> Bool
todosIguales [] = True
todosIguales (x:[]) = True
todosIguales (x1:x2:xs) = and [x1 == x2, todosIguales (x2:xs)]

-- EJERCICIO 5

data AB a = Nil | Bin (AB a) a (AB a)

vacioAB :: AB a -> Bool
vacioAB Nil = True
vacioAB (Bin _ _ _) = False

negacionAB :: AB Bool -> AB Bool
negacionAB Nil = Nil
negacionAB (Bin left p right) = Bin (negacionAB left) (not p) (negacionAB right)

productoAB :: AB Int -> Int
productoAB Nil = 1
productoAB (Bin left x right) = x * productoAB left * productoAB right