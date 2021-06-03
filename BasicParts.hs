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
data Row = Row Base Index deriving (Show)
data Column = Column Base Index deriving (Show)
data Cell = Cell Base Index deriving (Show)

-- Part and its instances
class Part a where
    getBase :: a -> Int
    getIndex :: a -> Int
    sameSquare :: a -> a -> Bool

instance Part Row where
    getBase (Row b _) = b
    getIndex (Row _ r) = r
    sameSquare (Row b1 _) (Row b2 _) = b1 == b2

instance Part Column where
    getBase (Column b _) = b
    getIndex (Column _ c) = c
    sameSquare (Column b1 _) (Column b2 _) = b1 == b2

instance Part Cell where
    getBase (Cell b _) = b
    getIndex (Cell _ i) = i
    sameSquare (Cell b1 _) (Cell b2 _) = b1 == b2

-- Eq instances of some Parts
instance Eq Row where
    x == y = areTheTwoRowsSame x y
    x /= y = not (areTheTwoRowsSame x y)

instance Eq Column where
    x == y = areTheTwoColsSame x y
    x /= y = not (areTheTwoColsSame x y)

instance Eq Cell where
    x == y = areTheTwoCellsSame x y
    x /= y = not (areTheTwoCellsSame x y)

-- Ord instances of some Parts
instance Ord Row where
    compare r1 r2
        | sameSquare r1 r2 && n1 > n2 = GT
        | getBase r1 > getBase r2 = GT
        | sameSquare r1 r2 && n1 < n2 = LT
        | getBase r1 < getBase r2 = LT
        | areTheTwoRowsSame r1 r2 = EQ
        where
            n1 = getIndex r1
            n2 = getIndex r2

instance Ord Column where
    compare c1 c2
        | sameSquare c1 c2 && n1 > n2 = GT
        | getBase c1 > getBase c2 = GT
        | sameSquare c1 c2 && n1 < n2 = LT
        | getBase c1 < getBase c2 = LT
        | areTheTwoColsSame c1 c2 = EQ
        where
            n1 = getIndex c1
            n2 = getIndex c2

instance Ord Cell where
    compare i1 i2
        | sameSquare i1 i2 && n1 > n2 = GT
        | getBase i1 > getBase i2 = GT
        | sameSquare i1 i2 && n1 < n2 = LT
        | getBase i1 < getBase i2 = LT
        | areTheTwoCellsSame i1 i2 = EQ
        where
            n1 = getIndex i1
            n2 = getIndex i2

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

areTheTwoCellsSame :: Cell -> Cell -> Bool
areTheTwoCellsSame i1 i2 = sameSquare i1 i2 && n1 == n2
    where
        n1 = getIndex i1
        n2 = getIndex i2

-- Checks for index validities

isCellValid :: Cell -> Bool
isCellValid (Cell b i) =  1 <= i && i <= b^2

isRowValid :: Row -> Bool
isRowValid (Row b r) = 1 <= r && r <= b

isColumnValid :: Column -> Bool
isColumnValid (Column b c) = 1 <= c && c <= b

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
areTheTwoRowsSame r1 r2 = sameSquare r1 r2 && n1 == n2
    where
        n1 = getIndex r1
        n2 = getIndex r2

areTheTwoColsSame :: Column -> Column -> Bool
areTheTwoColsSame c1 c2 = sameSquare c1 c2 && n1 == n2
    where
        n1 = getIndex c1
        n2 = getIndex c2

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