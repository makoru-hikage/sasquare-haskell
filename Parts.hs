module Parts(
    Index,
    Base,
    Row(..),
    Column(..),
    Cell(..),
    Part,
    getRow,
    getColumn,
    cellIndex,
    cellIndices,
    rowIndex,
    columnIndex,
    findCellByIndices,
    intersectRowColumn,
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
data Row = Row Base Index deriving (Ord, Eq, Show)
data Column = Column Base Index deriving (Ord, Eq, Show)
data Cell = Cell Base Index deriving (Ord, Eq, Show)

class Part a where
    getBase :: a -> Int

instance Part Row where
    getBase (Row b _) = b

instance Part Column where
    getBase (Column b _) = b

instance Part Cell where
    getBase (Cell b _) = b

-- Functions for the Cell

getRow :: Cell -> Row
getRow (Cell b i) = Row b r
    where r = ceiling (i % b)

getColumn :: Cell -> Column
getColumn (Cell b i) = Column b c
    where c = i + b - b * ceiling (i % b)

cellIndex :: Cell -> Int 
cellIndex (Cell _ i) = i

cellIndices :: [Cell] -> [Int]
cellIndices = map cellIndex

-- Line segment indices

rowIndex :: Row -> Int
rowIndex (Row _ r) = r

columnIndex :: Column -> Int
columnIndex (Column _ r) = r

-- Checks for index validities

isCellValid :: Cell -> Bool
isCellValid (Cell b i) =  1 <= i && i <= b^2

isRowValid :: Row -> Bool
isRowValid (Row b r) = 1 <= r && r <= b

isColumnValid :: Column -> Bool
isColumnValid (Column b r) = 1 <= r && r <= b

-- Intersection Functions

-- Find the cell using a row index and a column index
findCellByIndices :: Base -> Int -> Int -> Cell
findCellByIndices b r c = Cell b (b*r - b + c)

-- Find the intersection of a row and column
intersectRowColumn :: Row -> Column -> Maybe Cell
intersectRowColumn r c
    | getBase r /= getBase c = Nothing
    | not (isRowValid r && isColumnValid c) = Nothing
    | otherwise = Just $ findCellByIndices b ri ci
    where
        b = getBase r
        ri = rowIndex r
        ci = columnIndex c

rowCells :: Row -> [Cell]
rowCells r = map (findCellByIndices b ri) [1..b]
    where
        b = getBase r
        ri = rowIndex r

columnCells :: Column -> [Cell]
columnCells c = map (flip (findCellByIndices b) ci) [1..b]
    where
        b = getBase c
        ci = columnIndex c