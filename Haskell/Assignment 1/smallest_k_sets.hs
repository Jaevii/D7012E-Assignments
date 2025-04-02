-- Ludvig Järvi, ludjrv-1 --

-- Define Sublist as a type [(Int, Int, Int, [Int])]
data Sublist = Sublist {
    sumOfElements :: Int,
    startIndex :: Int,
    endIndex :: Int,
    elements :: [Int]
} deriving (Show)

-- Returns a list of all possible non-empty sublists
generateSublists :: [Int] -> [Sublist]
generateSublists xs = [Sublist
        (sumOfList sub) -- sumOfElements
        (i+1) (j+1) -- startIndex, endIndex
        sub -- elements
        | i <- [0..length xs-1], j <- [i..length xs-1] -- i, j
        , let sub = take (j-i+1) (drop i xs) -- sublist
    ]

-- Computes the sum of a sublist
sumOfList :: [Int] -> Int
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs

-- Sorts the list of sublists by the sum of the sublist using Quicksort
sortSublists :: [Sublist] -> [Sublist]
sortSublists [] = []
sortSublists (x:xs) = sortSublists smaller ++ [x] ++ sortSublists larger
    where
        smaller = [a | a <- xs, sumOfElements a <= sumOfElements x]
        larger = [a | a <- xs, sumOfElements a > sumOfElements x]

-- Prints a given list of strings
printList :: [String] -> String
printList [] = ""
printList (x:xs) = x ++ "\n" ++ printList xs

-- Stringifies a list of sublists into a list of strings
printSublists :: [Sublist] -> [String]
printSublists = map formatSublist 

-- Stringifies a sublist
formatSublist :: Sublist -> String
formatSublist s = show (sumOfElements s) ++ "\t" ++ show (startIndex s) ++ "\t" ++ show (endIndex s) ++ "\t" ++ show (elements s)

-- Function to print the smallest k sets of a list
smallestKsets :: [Int] -> Int -> IO()
smallestKsets xs k = putStr ("\nEntire list: " ++ show xs ++ "\n\nsize\ti\tj\tsublist\n" ++ printList (printSublists (take k (sortSublists (generateSublists xs)))) ++ "\n")

-- Test cases --
test1 = [x*(-1)^x | x <- [1..100]] :: [Int] -- k=15
test2 = [24,-11,-34,42,-24,7,-19,21] :: [Int] -- k=6
test3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] :: [Int] -- k=8

main :: IO()
main = do
    --smallestKsets test1 15 
    --smallestKsets test2 6 
    smallestKsets test3 8