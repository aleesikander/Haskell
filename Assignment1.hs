--                             Easy
-- Q1

targetSum :: (Ord a, Num a) => [a] -> a -> [[a]]
targetSum array target =
  [[a, b] | a <- array, b <- array, a >= b, a + b == target]




main :: IO ()
main = do
    print (targetSum [4,-1,0,1,-3,2,5,-6] 1)
    print (targetSum[1,2,3,4,5] 5) 
    print(targetSum[1,2,3,4,5,6] 10) 
    print(targetSum[1,2,3,4,5] 0) 
    print(targetSum[1,10,8,7,6,2,3,4,5,-1,9] 10)



