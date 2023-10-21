module HelperFunctions where

-- This helper function rounds down a Float to the nearest whole number.
floorFloat :: Float -> Int
floorFloat = fromIntegral . floor