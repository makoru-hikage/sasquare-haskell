import Parts

data DescendingSlant = DescendingSlant Base Index deriving (Eq, Ord, Show)
data AscendingSlant = AscendingSlant Base Index deriving (Eq, Ord, Show)

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

-- Slant index accessors
descendingIndex :: DescendingSlant -> Int
descendingIndex (DescendingSlant _ k) = k

ascendingIndex :: AscendingSlant -> Int
ascendingIndex (AscendingSlant _ u) = u

-- Get the two longest slants of each kind
diagonalCells :: Base -> [Cell]
diagonalCells b = map (\n -> findCellByIndices b n n) [1..b]

antidiagonalCells :: Base -> [Cell]
antidiagonalCells b = map (Cell b . (\n -> b^2 - b*n + n)) [1..b]

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
