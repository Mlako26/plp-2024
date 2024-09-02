merge :: (a -> a -> Bool) -> [a] -> [a] -> [a]
merge f l []          = l
merge f [] l          = l
merge f (x:xs) (y:ys) = if f x y
                        then x : merge f xs (y:ys)
                        else y : merge f (x:xs) ys

mergesort :: (a -> a -> Bool) -> [a] -> [a]
mergesort f [] = []
mergesort f (x:[]) = [x]
mergesort f x = coordinateMergeSort f [] x

coordinateMergeSort :: (a -> a -> Bool) -> [a] -> [a] -> [a]
coordinateMergeSort f [] (x:xs) = coordinateMergeSort f [x] xs
coordinateMergeSort f (x:xs) (y:ys) 
    | areHalfs (x:xs) (y:ys) = merge f (mergesort f (x:xs)) (mergesort f (y:ys))
    | otherwise              = coordinateMergeSort f (y:(x:xs)) ys

areHalfs :: [a] -> [a] -> Bool
areHalfs l1 l2 = abs (length l1 - (length l2)) <= 1

numberMerge :: [Int] -> [Int] -> [Int]
numberMerge l1 l2 = merge (<) l1 l2

numberMergesort :: [Int] -> [Int]
numberMergesort l1 = mergesort (<) l1