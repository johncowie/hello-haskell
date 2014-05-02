-- Problem 1
-- Find the last element in a list
myLast :: [a] -> a
myLast [y] = y
myLast (_:y) = myLast y

-- Problem 2
-- Find the last-but-one element in a list
myButLast :: [a] -> a
myButLast [e] = e
myButLast [e1, e2] = e1
myButLast (e:es) = myButLast es

-- Problem 3
-- Find the nth element in a list
myNth :: [a] -> Integer -> a
myNth (x:_) 0 = x
myNth (x:xs) i = (myNth xs (i - 1))

-- Problem 4
-- Find the number of elements in a list
mySize :: [a] -> Int
mySize [] = 0
mySize (x:xs) = 1 + mySize xs

-- Problem 5
-- Reverse a list
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = (myReverse xs) ++ [x]

-- Problem 6
-- Find out whether a list is a palindrome
palindrome :: (Eq a) => [a] -> Bool
palindrome [] = True
palindrome [a] = True
palindrome (x:xs) = (x == last xs) && palindrome (init xs)

-- Problem 7
-- Flatten a nested list structure
data NestedList a = Elem a | List [NestedList a]  -- NB data keyword is way of defining a new data type
                                                  -- can be element or list of nested lists (either elements or lists)
myFlatten :: NestedList a -> [a]
myFlatten (Elem x) = [x] -- if it's an element then just return the element
-- if it's a list then map myFlatten to each element and concatenate them all
myFlatten (List x) = (foldr (++) [] (map myFlatten x))


-- Problem 8
-- Compress duplicates in a collection
compress :: (Eq a) => [a] -> [a]
compress [x] = [x]
-- compress [x, y] = if (x == y) then [x] else [x, y]
compress (x:xs) = (if(x == (head xs)) then [] else [x]) ++ compress xs

-- Problem 9
-- pack consecutive values in a list into sublists
-- e.g. pack [1,2,2,3,3,4] => [[1], [2,2], [3,3], [4]]
pack :: (Eq a) => [a] -> [[a]]
pack [] = [[]]
pack [x] = [[x]]
pack (x:xs) = if (x == (head (head packed)))
              then ([([x] ++ head packed)] ++ (tail packed))
              else [[x]] ++ packed
  where packed = (pack xs)


-- Problem 10
-- run length encoding
-- e.g. encode "aaaabccaadeeee" => [(4, 'a'), (1, 'b'), (2, 'a'), (2, 'a'), (1, 'd'), (4, 'e')]
runEncode :: (Eq a) => [a] -> [(Int, a)]
runEncode [] = []
runEncode [x] = [(1, x)]
runEncode (x:xs) = if (x == (snd (head encoded)))
                   then [((1 + (fst (head encoded))), (snd (head encoded)))] ++ (tail encoded)
                   else [(1, x)] ++ encoded
  where encoded = runEncode xs
