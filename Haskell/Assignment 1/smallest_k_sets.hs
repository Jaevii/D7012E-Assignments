

threeDifferent :: Int -> Int -> Int-> Bool
threeDifferent x y z = (x/= y) && (y/=z) && (z/=x)


main :: IO()
main = do
    print (threeDifferent 1 2 1)