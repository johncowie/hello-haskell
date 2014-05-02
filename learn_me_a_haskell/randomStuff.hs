-- boat problem
boater :: [Int] -> [Int] -> Bool -> Int
boater [] _ _ = 0
boater xs ys right = if right
                     then -- move two from ys to xs
                     else [boater    | x <- xs]




-- the algorithm
   -- spawn a branch for moving each item from side to the other item from 1 list to the other
   -- need some sort of boolean flag for indicating the direction
   -- recursive function completes when 1st list is empty (all people are on the other side of the river)
