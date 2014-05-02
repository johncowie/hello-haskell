-- recursive maximum function
myMaximum :: (Ord a) => [a] -> a
myMaximum [] = error "maximum of empty list"
myMaximum [x] = x
myMaximum (x:xs)
  | x > maxTail = x
  | otherwise = maxTail
  where maxTail = myMaximum xs

-- replicate
myReplicate :: (Num i, Ord i) => i -> a -> [a]
myReplicate 0 _ = []
myReplicate n a = [a] ++ myReplicate (n - 1) a

-- take
myTake :: (Num i, Ord i) => i -> [a] -> [a]
myTake _ [] = []
myTake 0 _ = []
myTake n (x:xs) = [x] ++ myTake (n - 1) xs

-- reverse
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]


-- repeat
myRepeat :: a -> [a]
myRepeat x = [x] ++ repeat x

-- zip - takes two lists combines them into tuples
myZip :: [a] -> [a] -> [(a, a)]
myZip [] _ = []
myZip _ [] = []
myZip (x:xs) (y:ys) = (x, y):myZip xs ys

-- elem - takes an element and checks whether it's in the list
myElem :: (Eq a) => a -> [a] -> Bool
myElem _ [] = False
myElem a (x:xs) = (a == x) || myElem a xs

-- quicksort!
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort (x:xs) =
  let smallerSorted = quicksort [a | a <- xs, a <= x]
      biggerSorted = quicksort [a | a <- xs, a > x]
  in smallerSorted ++ [x] ++ biggerSorted
