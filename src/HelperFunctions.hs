module HelperFunctions where
-- This might not be conventional but i felt like this reduces the clutter
-- in the other files because they were getting big.

import Model


positionToTuple :: Position -> (Float, Float)
positionToTuple (Point x y) = (x, y)

-- This helper function rounds down a Float to the nearest whole number.
floorFloat :: Float -> Int
floorFloat flt = fromIntegral (floor flt)