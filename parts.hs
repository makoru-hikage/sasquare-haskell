import Data.Ratio
import Data.Ix

type Base = Int
type Index = Int

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

-- Accessors for the Rows and Columns

rowIndex :: Row -> Int
rowIndex (Row _ r) = r

columnIndex :: Column -> Int
columnIndex (Column _ r) = r

columnBase :: Column -> Int
columnBase (Column b _) = b

-- Checks for index validities

isCellValid :: Cell -> Bool
isCellValid (Cell b i) =  1 <= i && i <= b^2

isRowValid :: Row -> Bool
isRowValid (Row b r) = 1 <= r && r <= b

isColumnValid :: Column -> Bool
isColumnValid (Column b r) = 1 <= r && r <= b

-- Intersection Functions

-- Find the cell using a row index and a column index
-- findCellByIndices :: Base -> Int -> Int -> Cell
findCellByIndices :: Int -> Int -> Int -> Cell
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