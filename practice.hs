{-# OPTIONS_GHC -Wno-deferred-out-of-scope-variables #-}
import Data.Bits (FiniteBits(finiteBitSize))
import GHC.Base (VecElem(Int16ElemRep))
-- Function to double an integer
double :: Int -> Int
double x = x * 2

-- Checking whether an integer is even or not

isEven :: Int -> Bool
isEven x = if x `mod` 2 == 0 then True else False

-- Recursive function to check the factorial

factorial :: Int -> Int
factorial 0 = 1
factorial 1 = 1
factorial x = x * factorial (x-1)

-- Sum of a List

sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- Creating new data type
data Shape = Circle Float | Rectangle Float Float
area :: Shape -> Float
area (Circle r) = pi*r*r
area (Rectangle l w) = l *w

-- Using the zipWith function
-- 0 : 1 : zipWith (+) [1,2,3] [4,5,6] gives [0,1,5,7,9] as it takes 0 
-- and 1 to be the first and second values of the list and then adds
-- respective elements of the two lists together e.g 1+4, 2+5, 3+6

-- Fibonacci Sequence
fibonacci :: Int -> [Int]
fibonacci n = take n fibs 
        where
            fibs = 0 : 1 : zipWith (+) (fibs) (tail fibs)

-- Function to check prime number
isPrime :: Int -> Bool
isPrime n
    | n <=1 = False
    | otherwise = Nil [x | x<- [2..n-1], n `mod` x ==0]

-- Quicksort Algorithm
sortQuick :: Ord a => [a] -> [a]
sortQuick [] = []
sortQuick (x:xs) = sortQuick [y | y <- xs, y<x] ++ [x] ++ sortQuick [y | y<-xs, y>=x]

-- Swap function for tuples
swap :: (a,b) -> (b,a)
swap (x,y) = (y,x)

-- Class Questions
f1 list1 list2 = [(x,y)| x<-list1, y<-list2]
f2 list = [(id,name) | (id,name, address)<- list, id>0]

f3 list g = [g x | x <- list]
add1 x = x + 2

-- Bubble Sort
isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x]= True
isSorted (x:y:ys) = if x<=y then isSorted(y:ys) else False

bubble :: Ord a => [a] -> [a]
bubble [] = []
bubble [x] = [x]
bubble (x:y:ys) = if x<=y 
                            then x : bubble(y:ys) 
                            else y : bubble(x:ys)

bubbleSort :: Ord a => [a] -> [a]
bubbleSort list = if isSorted list
                            then list
                            else bubbleSort (bubble list)


-- Merge Sort

merge :: Ord a => [a] -> [a] -> [a]
merge [] list = list
merge list [] = list
merge (x:xs) (y:ys) = if x <= y
    then x: merge xs (y:ys)
    else y: merge (x:xs) ys


mergeSort :: Ord a => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort list = merge
                (mergeSort(take (length list `div` 2) list))
                (mergeSort(drop (length list `div` 2) list))

--Higher Order Functions
-- Passing functions as arguments to other functions
-- The function which receives another function as an argument is known as a higher order function

add2 :: Int -> Int -> Int
add2 x y= x + y 

addAnother = \x y -> x + y

mapNew f [] = []
mapNew f(x:xs) = f x : mapNew f xs

mapNew1 f list = [ f x | x <- list ] -- Easier way to apply function to map to another list, x such that
-- x belongs to the list and apply the function f to each element of the list
add3 = \x -> x +2
-- mapNew add3 causes add3 function to be applied to each element of the list that is provided as 
-- argument to the mapNew function, and it maps the list to another list

-- Functional Composition
sqr x = x*x
squareTwice x = square (sqr x)
squareTwice1 x = (sqr.sqr) x
squareTwice2  = sqr.sqr -- squareTwice2 infers from the type of the function after the = sign
firstAddthenSquare = sqr.add3 






main :: IO ()
main = do
    print (double 2)
    print (isEven 4)
    print (factorial 4)
    print (sumList [1,2,3,4])
    let areaCircle = area (Circle 5)
    let areaRectangle = area (Rectangle 4 5)
    putStrLn("The area of the circle is: " ++ show areaCircle)
    putStrLn("The area of the rectangle is: " ++ show areaRectangle)
    print (fibonacci 5)
    print (isPrime 5)
    putStrLn("Please enter an integer: ")
    inputStr <- getLine
    let num = read inputStr :: Int 
    let result = double num
    putStrLn("The doubled integer would be: " ++ show result)

    putStrLn("Please enter an integer to check if its prime: ")
    input <- getLine
    let num1 = read input :: Int 
    if isPrime num1
        then putStrLn "The integer is a prime number"
        else putStrLn "The integer is not a prime number"

    print(swap(2,3))
    
