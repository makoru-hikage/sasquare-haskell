import BasicParts
import Data.List

-- Derived from
-- stackoverflow.com/questions/12876384/
groupIntoN :: Int -> [a] -> [[a]]
groupIntoN _ [] = []
groupIntoN n l
  | n > 0 = take n l : groupIntoN n (drop n l)
  | otherwise = error "n must be positive!"

-- Group a list with length of b^2 into b
splitSquareList :: [a] -> [[a]]
splitSquareList [] = []
splitSquareList l
    | isSquareValid (length l) = groupIntoN root l
    | otherwise = [l]
        where
            root = head $ filter (\x -> n == x^2) [1..n]
            n = length l

-- Creates a list of Ints from 1 to (Base ^ 2)
createSquareIntList :: Base -> [Int]
createSquareIntList b = map getIndex $ allCells b

-- Left-pad a string with spaces 
padStrByBase :: Int -> String -> String
padStrByBase b s = foldr (++) s lengthOfPadding
    where
        nDigits = length s -- Number of digits of n
        squareDigits = length $ show (b^2)
        lengthOfPadding = replicate (squareDigits - nDigits) " "

-- Retain Int in String form if...
retainNumInStrIf :: (Int -> Bool) -> Int -> String
retainNumInStrIf p n
     | p n = show n
     | otherwise = "*"

-- The String representation of an Int Square
-- [Index] is selected Cell Indices
presentSquare :: Base -> [Index] -> String
presentSquare b i = square
    where
        square =(
            unlines -- add \n per multiples of b
            . map unwords -- separate elements by space
            . splitSquareList
            . map (padStrByBase b . retainNumInStrIf cellIsSelected)
            . createSquareIntList) b
        cellIsSelected = (`elem` i)

-- TODO: Main and its GetOpt
