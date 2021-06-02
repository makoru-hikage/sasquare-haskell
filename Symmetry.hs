import Data.Ratio ( (%) )
import Data.Tuple ()
import Parts (
    Base,
    Row(..),
    Column(..),
    Cell(..),
    cellIndex,
    getRow,
    getColumn,
    getIndex,
    getBase,
    rowCells,
    isCellValid,
    isRowValid,
    isColumnValid,
    findCellByIndices,
    nthCellOfRow,
    nthCellOfColumn
    )

-- Maps a function to each element of the pair
both :: (a -> b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

-- Find the centermost element in a finite list of an odd length
oddCenterIndex :: Int -> Int
oddCenterIndex x = ceiling (x % 2)

-- Find the pair of centermost elements 
-- in a finite list of an even length
evenCenterIndices :: Int -> (Int,Int)
evenCenterIndices x = (mainCenterIndex, secondaryCellIndex)
    where
        mainCenterIndex = ceiling (x % 2)
        secondaryCellIndex = mainCenterIndex + 1

-- Find the opposite element of a 
-- chosen element from the opposite side
oppositeIndex :: Int -> Int -> Int
oppositeIndex l x = l + 1 - x

pairOfOpposites :: Int -> Int -> (Int, Int)
pairOfOpposites l x = (x, oppositeIndex l x)

-- Find the center cell of a row
rowCenter :: Row -> [Maybe Cell]
rowCenter r
    | isRowValid r && odd b = [oddCenter]
    | isRowValid r && even b = [fst evenCenter, snd evenCenter]
    | otherwise = [Nothing]
    where
        b = getBase r
        ri = getIndex r
        oddCenter = findCellByIndices b ri (oddCenterIndex b)
        evenCenter = both (nthCellOfRow r) (evenCenterIndices b)

-- Find the center cell of a column
columnCenter :: Column -> [Maybe Cell]
columnCenter c
    | isColumnValid c && odd b = [oddCenter]
    | isColumnValid c && even b = [fst evenCenter, snd evenCenter]
    | otherwise = [Nothing]
    where
        b = getBase c
        ri = getIndex c
        oddCenter = findCellByIndices b ri (oddCenterIndex b)
        evenCenter = both (nthCellOfColumn c) (evenCenterIndices b)

-- Find the center cell of a square if the base is odd
-- Find the four center cells of a square if the base is even
squareCenter :: Base -> [Maybe Cell]
squareCenter b
    | b < 1 = [Nothing]
    | odd b = [oddCenter]
    | even b = map (Just . Cell b) evenCenterIndices
    where
        oddCenter =
            findCellByIndices b (oddCenterIndex b) (oddCenterIndex b)
        evenCenterIndices = [
            cellIndex oddCenter,
            cellIndex oddCenter + 1,
            cellIndex oddCenter + b,
            cellIndex oddCenter + b + 1
            ]

-- Find the opposite cell of a cell along their row
horizontalOpposite :: Cell -> Maybe Cell
horizontalOpposite i
    | isCellValid i = nthCellOfRow r (oppositeIndex b c)
    | otherwise = Nothing 
    where
        b = getBase i
        r = getRow i
        c = getIndex (getColumn i)

-- Find the opposite cell of a cell along their column
verticalOpposite :: Cell -> Maybe Cell
verticalOpposite i
    | isCellValid i = nthCellOfColumn c (oppositeIndex b r)
    | otherwise = Nothing 
    where
        b = getBase i
        r = getIndex (getRow i)
        c = getColumn i

-- Find the opposite cell of a cell along their descending slant
descendingOpposite :: Cell -> Maybe Cell
descendingOpposite i
    | isCellValid i = findCellByIndices b r c
    | otherwise = Nothing
    where
        b = getBase i
        -- Find the opposite of the row and column indices
        r' = oppositeIndex b $ getIndex $ getRow i
        c' = oppositeIndex b $ getIndex $ getColumn i
        s = r' + c'
        -- Row Index of the opposite cell
        r = s - r'
        -- Column Index of the opposite cell
        c = s - c'

-- Find the opposite cell of a cell along their ascending slant
ascendingOpposite :: Cell -> Maybe Cell
ascendingOpposite i
    | isCellValid i = findCellByIndices b r c
    | otherwise = Nothing
    where
        b = getBase i
        -- The row and column indices are merely swapped
        r = getIndex $ getColumn i
        c = getIndex $ getRow i 
