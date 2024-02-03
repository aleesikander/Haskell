--                             Easy
-- Q1

targetSum :: (Ord a, Num a) => [a] -> a -> [(a,a)]
targetSum array target = sortByFirstElement $ filter meetsconditions allPairs
            where
                meetsconditions (a,b) = a +b == target
                allPairs = [(x,y) | x <- array, y <- array, x>=y]

sortByFirstElement :: Ord a => [(a,a)] -> [(a,a)]
sortByFirstElement [] = []
sortByFirstElement (x:xs) = sortByFirstElement smaller ++ [x] ++ sortByFirstElement larger
            where
                smaller = [a|a<-xs,fst a <= fst x]
                larger = [a|a<-xs, fst a > fst x]


main :: IO ()
main = do
    print (targetSum [4,-1,0,1,-3,2,5,-6] 1)
    print (targetSum[1,2,3,4,5] 5) 
    print(targetSum[1,2,3,4,5,6] 10) 
    print(targetSum[1,2,3,4,5] 0) 
    print(targetSum[1,10,8,7,6,2,3,4,5,-1,9] 10)



