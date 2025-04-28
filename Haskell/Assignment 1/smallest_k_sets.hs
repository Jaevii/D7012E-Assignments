-- Ludvig Järvi, ludjrv-1 --

-- Define Subset as a type [(Int, Int, Int, [Int])]
data Subset = Subset {
    sumOfElements :: Int,
    startIndex :: Int,
    endIndex :: Int,
    elements :: [Int]
} deriving (Show)

-- Generates a list of all possible non-empty Subsets using list comprehension
generateSubsets :: [Int] -> [Subset]
generateSubsets xs = [Subset
        (sumOfList sub) -- sumOfElements
        (i+1) (j+1) -- startIndex, endIndex
        sub -- elements
        | i <- [0..length xs-1], j <- [i..length xs-1] -- i, j
        , let sub = grabFirst (j-i+1) (grabAfter i xs) -- Subset
    ]

-- Recursively generates a list of all possible non-empty Subsets
generateSubsetsRec :: [Int] -> Int -> [Subset]
generateSubsetsRec [] _ = [] -- Base case: empty list
generateSubsetsRec xs i | i >= length xs = [] -- Base case: index out of bounds
generateSubsetsRec xs i = [Subset
        (sumOfList sub) -- sumOfElements
        (i+1) (j+1) -- startIndex, endIndex
        sub -- elements
        | j <- [i..length xs-1] -- j
        , let sub = grabFirst (j-i+1) (grabAfter i xs) -- Subset
    ] ++ generateSubsetsRec xs (i+1)

-- Computes the sum of a Subset
sumOfList :: [Int] -> Int
sumOfList [] = 0
sumOfList (x:xs) = x + sumOfList xs

-- Sorts the list of Subsets by the sum of the Subset using Quicksort
sortSubsets :: [Subset] -> [Subset]
sortSubsets [] = []
sortSubsets (x:xs) = sortSubsets smaller ++ [x] ++ sortSubsets larger
    where
        smaller = [a | a <- xs, sumOfElements a <= sumOfElements x]
        larger = [a | a <- xs, sumOfElements a > sumOfElements x]

-- Prints a given list of strings
printList :: [String] -> String
printList [] = ""
printList (x:xs) = x ++ "\n" ++ printList xs

-- Stringifies a list of Subsets into a list of strings
printSubsets :: [Subset] -> [String]
printSubsets = map formatSubset 

-- Stringifies a Subset
formatSubset :: Subset -> String
formatSubset s = show (sumOfElements s) ++ "\t" ++ show (startIndex s) ++ "\t" ++ show (endIndex s) ++ "\t" ++ show (elements s)

-- Returns the n first elements of a list (recursive take)
grabFirst :: Int -> [a] -> [a]
grabFirst 0 _ = []
grabFirst _ [] = []
grabFirst n (x:xs) = x : grabFirst (n-1) xs

-- Returns the elements after the first n elements of a list (recursive drop)
grabAfter :: Int -> [a] -> [a]
grabAfter 0 xs = xs
grabAfter _ [] = []
grabAfter n (_:xs) = grabAfter (n-1) xs

-- Function to print the smallest k sets of a list
smallestKsets :: [Int] -> Int -> IO()
smallestKsets xs k = putStr ("\nEntire list: " ++ show xs ++ "\n\nsize\ti\tj\tSubset\n" ++ printList (printSubsets (grabFirst k (sortSubsets (generateSubsets xs)))) ++ "\n")

-- Test cases --
test1 = [x*(-1)^x | x <- [1..100]] :: [Int] -- k=15
test2 = [24,-11,-34,42,-24,7,-19,21] :: [Int] -- k=6
test3 = [3,2,-4,3,2,-5,-2,2,3,-3,2,-5,6,-2,2,3] :: [Int] -- k=8

main :: IO()
main = do
    smallestKsets test2 6
    smallestKsets test3 8
