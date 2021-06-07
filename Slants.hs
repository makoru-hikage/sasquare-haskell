import Data.Ratio ( (%) )
import Data.List ( nub )
import BasicParts(
    Base,
    Row(..),
    Column(..),
    Cell(..),
    Index,
    Part(..),
    CellGroup(..),
    cellIndex,
    cellIndices, --added for interactive apps like ghci
    findCellByIndices,
    getColumn,
    getRow
    )

data DescendingSlant = DescendingSlant Base Index deriving (Show)
data AscendingSlant = AscendingSlant Base Index deriving (Show)

class Slant a where
    slantCardinality :: a -> Int
    slantIndex :: a -> Int
    isSlantValid :: a -> Bool

instance Part DescendingSlant where
    getBase (DescendingSlant b _) = b
    getIndex = slantIndex
    sameSquare (DescendingSlant b1 _) (DescendingSlant b2 _) =
        b1 == b2

instance Part AscendingSlant where
    getBase (AscendingSlant b _) = b
    getIndex = slantIndex
    sameSquare (AscendingSlant b1 _) (AscendingSlant b2 _) =
        b1 == b2

instance Slant DescendingSlant where
    slantIndex (DescendingSlant _ x) = x
    slantCardinality (DescendingSlant b x) = b - abs (b - x)
    isSlantValid (DescendingSlant b x) =
        x >= 1 && x <= countSlantsInSquare b

instance Slant AscendingSlant where
    slantIndex (AscendingSlant _ x) = x
    slantCardinality (AscendingSlant b x) = b - abs (b - x)
    isSlantValid (AscendingSlant b x) =
        x >= 1 && x <= countSlantsInSquare b

instance CellGroup DescendingSlant where
    isCellIn i x = areTheSlantsSame (getDescendingSlant i) x

instance CellGroup AscendingSlant where
    isCellIn i x = areTheSlantsSame (getAscendingSlant i) x

-- Counts the total number of slants in a square per kind
countSlantsInSquare :: Base -> Int
countSlantsInSquare b = 2*b - 1

-- An equation used to find the ascending slant of a cell
intersectionSum :: Cell -> Int
intersectionSum i = getIndex (getRow i) + getIndex (getColumn i)

-- An equation used to find the descending slant of a cell
intersectionDiff :: Cell -> Int
intersectionDiff i = getIndex (getColumn i) - getIndex (getRow i)

rIntersectionDiff :: Cell -> Int
rIntersectionDiff i = getIndex (getRow i) - getIndex (getColumn i)

-- Find which of the ascending or descending slant a cell belongs to
getDescendingSlant :: Cell -> DescendingSlant
getDescendingSlant i = DescendingSlant b (b + intersectionDiff i)
    where b = getBase i

getAscendingSlant :: Cell -> AscendingSlant
getAscendingSlant i = AscendingSlant (getBase i) (intersectionSum i - 1)

-- Used to list a cell index of the nth cell of the diagonal
diagonalFunc :: Base -> Int -> Int
diagonalFunc b n = cellIndex (findCellByIndices b n n)

-- Used to list a cell index of the nth cell of the antidiagonal
antidiagonalFunc :: Base -> Int -> Int
antidiagonalFunc b n = b^2 - b*n + n

diagonalCells :: Base -> [Cell]
diagonalCells b = map (Cell b . diagonalFunc b) [1..b]

antidiagonalCells :: Base -> [Cell]
antidiagonalCells b = map (Cell b . antidiagonalFunc b) [1..b]

xOfTheSquare :: Base -> [Cell]
xOfTheSquare b =  nub $ concatMap ($b) [diagonalCells, antidiagonalCells]

-- Predicate functions involving the diagonal
isSubdiagonal :: Cell -> Bool
isSubdiagonal i = intersectionDiff i < 0

isSuperdiagonal :: Cell -> Bool
isSuperdiagonal i = intersectionDiff i > 0

isDiagonal :: Cell -> Bool
isDiagonal i = intersectionDiff i == 0

isOffDiagonal :: Cell -> Bool
isOffDiagonal i = isSubdiagonal i || isSuperdiagonal i

-- Predicate functions involving the antidiagonal
isAntiSubdiagonal :: Cell -> Bool
isAntiSubdiagonal i = intersectionSum i > (1 + getBase i)

isAntiSuperdiagonal :: Cell -> Bool
isAntiSuperdiagonal i = intersectionSum i < (1 + getBase i)

isAntiDiagonal :: Cell -> Bool
isAntiDiagonal i = intersectionSum i == (1 + getBase i)

isAntiOffDiagonal :: Cell -> Bool
isAntiOffDiagonal i = isAntiSubdiagonal i || isAntiSuperdiagonal i

-- Slants comparison function
areTheSlantsSame :: (Slant a, Part a) => a -> a -> Bool
areTheSlantsSame x1 x2 = sameSquare x1 x2 && n1 == n2
    where
        n1 = getIndex x1
        n2 = getIndex x2

-- Special functions with the following traits
-- - when x = b then f(b,x) = 0
-- - when `1 leq x leq b` then f(b,x) = b^2 - b*x

-- FunctionU has the following traits
-- - when x = 2*b then u(b,x) = b
-- - when `1 leq x leq 2*b` then u(b,x) = x - b
-- - when x = 0 then u(b,x) = b^3
functionU :: Base -> Index -> Int
functionU b x = b ^ (2 + floor (-(x % b))) * abs (b - x)

-- FunctionW has the following traits
-- - when `1 leq x < 2*b` then w(b,x) = b - x
-- - when x = 0 then u(b,x) = b^2
functionW :: Base -> Index -> Int
functionW b x = abs (b - x) * (b - 1) + (b ^ floor (x % b)) * (b - x)

-- Cell pinpointing functions for the slants

-- Get the nth cell of a descending slant
nthCellOfDescSlant :: DescendingSlant -> Index -> Maybe Cell
nthCellOfDescSlant d n
    | isSlantValid d = Just $ Cell b (diagonalFunc b n + functionU b x)
    | otherwise = Nothing
    where
        b = getBase d
        x = slantIndex d

-- Get the nth cell of an ascending slant
nthCellOfAscSlant :: AscendingSlant -> Index -> Maybe Cell
nthCellOfAscSlant a n
    | isSlantValid a =
        Just $ Cell b (antidiagonalFunc b n - functionW b x)
    | otherwise = Nothing
    where
        b = getBase a
        x = slantIndex a

-- Cell listing functions for the slants

-- List all the cells of a descending slant
descendingSlantCells :: DescendingSlant -> [Maybe Cell]
descendingSlantCells a = map (nthCellOfDescSlant a) [1..n]
    where n = slantCardinality a

-- List all the cells of an ascending slant
ascendingSlantCells :: AscendingSlant -> [Maybe Cell]
ascendingSlantCells a = map (nthCellOfAscSlant a) [1..n]
    where n = slantCardinality a
