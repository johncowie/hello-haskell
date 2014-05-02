-- Problem 21
-- Insert an element at a given position in a list
insertAt :: a -> [a] -> Int -> [a]
insertAt a [] _ = [a]
insertAt a (x:xs) 1 = a:x:xs
insertAt a (x:xs) n = x:(insertAt a xs (n - 1))


-- Problem 22
-- Create a list containing all integers within a given range
range :: Int -> Int -> [Int]
range i k = if (i > k) then [] else [i] ++ (range (i + 1) k)


-- Problem 23
-- Extract a given number of random elements from a list
