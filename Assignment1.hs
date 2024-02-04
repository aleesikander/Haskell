--                             Easy
-- Q1

insertSort :: Ord a => [(a, a)] -> [(a, a)]
insertSort [] = []
insertSort (x:xs) = insert x (insertSort xs)
  where
    insert y [] = [y]
    insert y (z:zs)
      | fst y < fst z = y : z : zs
      | otherwise = z : insert y zs

-- Function to find target sum pairs with ordered elements, without imports
targetSum :: (Ord a, Num a) => [a] -> a -> [(a, a)]
targetSum array target = insertSort [(a, b) | a <- array, b <- array, a >= b, a + b == target]




main :: IO ()
main = do
    print (targetSum [4,-1,0,1,-3,2,5,-6] 1)
    print (targetSum[1,2,3,4,5] 5) 
    print(targetSum[1,2,3,4,5,6] 10) 
    print(targetSum[1,2,3,4,5] 0) 
    print(targetSum[1,10,8,7,6,2,3,4,5,-1,9] 10)



