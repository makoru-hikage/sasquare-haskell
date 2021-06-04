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

-- Left-pad a number with spaces 
padNumberByBase :: Int -> Int -> String
padNumberByBase b n = foldr (++) (show n) lengthOfPadding
    where
        nDigits = length $ show n -- Number of digits of n
        squareDigits = length $ show (b^2)
        lengthOfPadding = replicate (squareDigits - nDigits) " "

-- The String representation of an Int Square
presentSquare :: Base -> String
presentSquare b = square
    where
        square =(
            unlines -- add \n per multiples of b
            . map unwords -- separate elements by space
            . splitSquareList
            . map (padNumberByBase b)
            . createSquareIntList) b

-- TODO: Main and its GetOpt
