-- zipWith, zips two lists and applies a function to the two values in the lists
myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith _ [] _ = []
myZipWith _ _ [] = []
myZipWith f (x:xs) (y:ys) = [f x y] ++ myZipWith f xs ys
-- e.g. myZipWith (+) [1, 2, 3] [5, 6, 7] ==> [6, 8, 10]

-- flip, returns a function like the original function, with the arguments flipped
myFlip :: (a -> b -> c) -> b -> a -> c
myFlip f x y = f y x

-- map, maps a function onto each item in a list
myMap :: (a -> b) -> [a] -> [b]
myMap _ [] = []
myMap f (x:xs) = f x : myMap f xs

-- filter, removes items from the list if they don't match the predicate
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ [] = []
myFilter p (x:xs)
  | p x         = x : myFilter p xs
  | otherwise   = myFilter p xs

-- quicksort using filter
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort (filter (<= x) xs)
      biggerSorted =  quicksort (filter (> x) xs)
  in smallerSorted ++ [x] ++ biggerSorted

-- find the largest number under 100,000 that's divisible by 3829
largestDivisible :: (Integral a) => a
largestDivisible = head (filter p [100000, 99999..])
  where p x = x `mod` 3829 == 0

-- find the sum of all odd squares under 10000
sumOddSquares :: (Integral a) => a
sumOddSquares = sum (takeWhile (<10000) (filter odd (map (^2) [1..])))

-- Collatz - if number even, divide by 2, if odd multiply by 3 and add one - repeat until 1 is reached
-- for all starting numbers between 1 and 100, how many produce chains over 15 in length
collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n
  | even n   = n:collatz (n `div` 2)
  | odd n    = n:collatz (n * 3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map collatz [1..100]))
  where isLong xs = length xs > 15


---------------
--- Lambdas ---
---------------

-- use \ notation

numLongChainsLambda :: Int
numLongChainsLambda = length (filter (\xs -> length xs > 15) (map collatz [1..100]))



---------------
--- Folds -----
---------------

---- Left Fold ----

-- sum
mySum :: (Num a) => [a] -> a
mySum xs = foldl (\acc x -> acc + x) 0 xs

-- elem
myElem :: (Eq a) => a -> [a] -> Bool
myElem x xs = foldl (\acc n -> if (n == x) then True else acc) False xs  -- if a match then return True else return new curried function


---- Right Fold ----

mapWithFold :: (a -> b) -> [a] -> [b]
mapWithFold f xs = foldr (\x acc -> f x : acc) [] xs
