module BasicParts(
    Index,
    Base,
    Row(..),
    Column(..),
    Cell(..),
    Part(..),
    getRow,
    getColumn,
    getRowColumnPair,
    isCellValid,
    isRowValid,
    isColumnValid,
    cellIndex,
    cellIndices,
    findCellByIndices,
    intersectRowColumn,
    nthCellOfRow,
    nthCellOfColumn,
    rowCells,
    columnCells
) where

import Data.Ratio ( (%) )
import Data.Ix ()

-- A square always has a base
-- The base is the root of a perfect square
type Base = Int

-- An index is always an integer
type Index = Int

--TODO: Eq and Ord instances
data Row = Row Base Index deriving (Ord, Show)
data Column = Column Base Index deriving (Show)
data Cell = Cell Base Index deriving (Show)

-- Part and its instances
class Part a where
    getBase :: a -> Int
    getIndex :: a -> Int

instance Part Row where
    getBase (Row b _) = b
    getIndex (Row _ r) = r

instance Part Column where
    getBase (Column b _) = b
    getIndex (Column _ c) = c

instance Part Cell where
    getBase (Cell b _) = b
    getIndex (Cell _ i) = i

-- Eq instances of some Parts
instance Eq Row where
    x == y = areTheTwoRowsSame x y
    x /= y = not (areTheTwoRowsSame x y)

-- Cell functions
getRow :: Cell -> Row
getRow (Cell b i) = Row b r
    where r = ceiling (i % b)

getColumn :: Cell -> Column
getColumn (Cell b i) = Column b c
    where c = i + b - b * ceiling (i % b)

getRowColumnPair :: Cell -> (Row, Column)
getRowColumnPair i = (getRow i, getColumn i)

cellIndex :: Maybe Cell -> Int
cellIndex (Just (Cell _ i)) = i
cellIndex Nothing = 0

cellIndices :: [Maybe Cell] -> [Int]
cellIndices = map cellIndex

-- Checks for index validities

isCellValid :: Cell -> Bool
isCellValid (Cell b i) =  1 <= i && i <= b^2

isRowValid :: Row -> Bool
isRowValid (Row b r) = 1 <= r && r <= b

isColumnValid :: Column -> Bool
isColumnValid (Column b r) = 1 <= r && r <= b

-- Intersection Functions

-- Find the cell using a row index and a column index
findCellByIndices :: Base -> Int -> Int -> Maybe Cell
findCellByIndices b r c
    | c == 0 || r == 0 = Nothing
    | otherwise = Just (Cell b (b*r - b + c))

-- Find the intersection of a row and column
intersectRowColumn :: Row -> Column -> Maybe Cell
intersectRowColumn r c
    | getBase r /= getBase c = Nothing
    | isRowValid r && isColumnValid c = findCellByIndices b ri ci
    | otherwise = Nothing
    where
        b = getBase r
        ri = getIndex r
        ci = getIndex c

-- Row functions
nthCellOfRow :: Row -> Index -> Maybe Cell
nthCellOfRow r n
    | isRowValid r = findCellByIndices b ri n
    | otherwise = Nothing
    where 
        b = getBase r
        ri = getIndex r

rowCells :: Row -> [Maybe Cell]
rowCells r = map (findCellByIndices b ri) [1..b]
    where
        b = getBase r
        ri = getIndex r

areTheTwoRowsSame :: Row -> Row -> Bool
areTheTwoRowsSame r1 r2 = b1 == b2 && n1 == n2
    where
        b1 = getBase r1
        b2 = getBase r2
        n1 = getIndex r1
        n2 = getIndex r2

-- Column functions
nthCellOfColumn :: Column -> Index -> Maybe Cell
nthCellOfColumn c n
    | isColumnValid c = findCellByIndices b n ci
    | otherwise = Nothing
    where 
        b = getBase c
        ci = getIndex c

columnCells :: Column -> [Maybe Cell]
columnCells c = map (flip (findCellByIndices b) ci) [1..b]
    where
        b = getBase c
        ci = getIndex c