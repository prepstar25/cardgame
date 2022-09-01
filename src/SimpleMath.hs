module SimpleMath(add,addM,subM) where

import Types

add :: (Int , Int) -> Int
add (a,b) = a + b 

addM :: (Money , Money) -> Money
addM (Money a, Money b) = Money (a + b)

subM :: (Money , Money) -> Money
subM (Money a, Money b) = Money (a - b)

