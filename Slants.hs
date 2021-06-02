import Data.Ratio ( (%) )
import Parts

data DescendingSlant = DescendingSlant Base Index deriving (Eq, Ord, Show)
data AscendingSlant = AscendingSlant Base Index deriving (Eq, Ord, Show)

class Slant a where
    slantCardinality :: a -> Int
    slantIndex :: a -> Int

instance Part DescendingSlant where
    getBase (DescendingSlant b _) = b

instance Part AscendingSlant where
    getBase (AscendingSlant b _) = b

instance Slant DescendingSlant where
    slantIndex (DescendingSlant _ x) = x
    slantCardinality (DescendingSlant b x) = b - abs (b - x)

instance Slant AscendingSlant where
    slantIndex (AscendingSlant _ x) = x
    slantCardinality (AscendingSlant b x) = b - abs (b - x)

-- An equation used to find the ascending slant of a cell
intersectionSum :: Cell -> Int
intersectionSum i = rowIndex (getRow i) + columnIndex (getColumn i)

-- An equation used to find the descending slant of a cell
intersectionDiff :: Cell -> Int
intersectionDiff i = columnIndex (getColumn i) - rowIndex (getRow i)

rIntersectionDiff :: Cell -> Int
rIntersectionDiff i = rowIndex (getRow i) - columnIndex (getColumn i)

-- Find which of the ascending or descending slant a cell belongs to
getDescendingSlant :: Cell -> DescendingSlant
getDescendingSlant i = DescendingSlant b (b + intersectionDiff i)
    where b = getBase i

getAscendingSlant :: Cell -> AscendingSlant
getAscendingSlant i = AscendingSlant (getBase i) (intersectionSum i - 1)

-- Get the two longest slants of each kind

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

-- Predicate functions with regards to the kind of slant
isSubdiagonal :: Cell -> Bool
isSubdiagonal i = intersectionDiff i < 0

isSuperdiagonal :: Cell -> Bool
isSuperdiagonal i = intersectionDiff i > 0

isDiagonal :: Cell -> Bool
isDiagonal i = intersectionDiff i == 0

isOffDiagonal :: Cell -> Bool
isOffDiagonal i = isSubdiagonal i || isSuperdiagonal i

isAntiSubdiagonal :: Cell -> Bool
isAntiSubdiagonal i = intersectionSum i > (1 + getBase i)

isAntiSuperdiagonal :: Cell -> Bool
isAntiSuperdiagonal i = intersectionSum i < (1 + getBase i)

isAntiDiagonal :: Cell -> Bool
isAntiDiagonal i = intersectionSum i == (1 + getBase i)

isAntiOffDiagonal :: Cell -> Bool
isAntiOffDiagonal i = isAntiSubdiagonal i || isAntiSuperdiagonal i

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