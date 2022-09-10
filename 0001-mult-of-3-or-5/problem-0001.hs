
main :: IO ()
main = do
    let allNums = [ 1..999 ]
        divBy3  = [ x | x <- allNums, x `mod` 3 == 0 ]
        divBy5  = [ x | x <- allNums, x `mod` 5 == 0 ]
        clean5  = filter (\n->not $ n `elem` divBy3) divBy5
        sumMe   = divBy3 ++ clean5

    print $ sum sumMe
