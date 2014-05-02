-- Problem 11
-- run length encoding where singles don't have number
-- e.g. encode "aaaabccaadeeee" => [Multiple 4 'a', Single 'b', Multiple 2 'c', Multiple 2 'a', Single 'd', Multiple 4 'e']
runEncode :: (Eq a) => [a] -> [(Int, a)]
runEncode [] = []
runEncode [x] = [(1, x)]
runEncode (x:xs) = if (x == (snd (head encoded)))
                   then [((1 + (fst (head encoded))), (snd (head encoded)))] ++ (tail encoded)
                   else [(1, x)] ++ encoded
  where encoded = runEncode xs

data ListItem a = Single a | Multiple Int a
  deriving (Show)

runEncodeMod :: (Eq a) => [a] -> [ListItem a]
runEncodeMod (x) = map encodeConvert (runEncode x)
  where
    encodeConvert (1, a) = Single a
    encodeConvert (x, a) = Multiple x a



-- Problem 12
-- take the results of problem 11 and convert back to string
-- need to find some way of getting at the data inside the type
runDecode :: (Eq a) => [ListItem a] -> [a]
runDecode [(Single a)] = [a]
runDecode [(Multiple x a)] = (take x (repeat a))
runDecode (x:xs) = (runDecode [x]) ++ (runDecode xs)


-- Problem 13 -- my solution of 10 is also the solution for this


-- Problem 14
-- Duplicate the elements of a list

duplic :: [a] -> [a]
duplic [] = []
duplic (x:xs) = [x, x] ++ duplic xs


-- Problem 15
-- Replicate the elements of a list a given number of times

replic :: [a] -> Int -> [a]
replic [] n = []
replic (x:xs) n = (take n (repeat x)) ++ replic xs n

-- Problem 16
-- drop every nth element from a list

dropNth :: [a] -> Int -> [a]
dropNth [] _ = []
dropNth (xs) n = (take (n - 1) xs) ++ dropNth (drop n xs) n




-- Problem 17
-- split list into two parts
-- do not use any predefined predicates
split :: [a] -> Int -> ([a], [a])
split [] _ = ([], [])
split (x:xs) n = if (n > 0)
                 then ([x] ++ fst splitted, snd splitted)
                 else (fst splitted, [x] ++ snd splitted)
   where splitted = split xs (n - 1)



-- Problem 18
-- take a slice from a list, given a start and end index
slice :: [a] -> Int -> Int -> [a]
slice [] _ _ = []
slice (x) 1 0 = []
slice (x:xs) 1 k = x:slice xs 1 (k - 1)
slice (x:xs) i k = slice xs (i - 1) (k - 1)



-- Problem 19
-- Rotate a list N places to the left
rotate :: [a] -> Int -> [a]
rotate (x) 0 = x
rotate (x:xs) n = rotate (xs ++ [x]) (m - 1)
  where m = (if (n < 0) then ((length xs + 1) + n) else n)

-- Problem 20
-- Remove the kth element from a list
removeAt :: [a] -> Int -> [a]
removeAt [] _ = []
removeAt (x:xs) 1 = removeAt xs 0
removeAt (x:xs) 0 = x:(removeAt xs 0)
removeAt (x:xs) n = x:(removeAt xs (n - 1))
