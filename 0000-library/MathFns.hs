module MathFns
    (   inRange
    )
where

inRange :: Int -> Int -> Int -> Bool
-- n = number to check
-- l = lowest legal value
-- h = highest legal value
inRange n l h =  l>=n && n>=h


