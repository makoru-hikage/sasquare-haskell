import Data.Ratio ( (%) )
import Data.Tuple ()
import Parts (
    Base,
    Row(..),
    Column(..),
    Cell(..),
    cellIndex,
    rowCells,
    isRowValid,
    isColumnValid,
    findCellByIndices,
    nthCellOfRow,
    nthCellOfColumn,
    getBase,
    rowIndex, columnIndex
    )

-- Maps a function to each element of the pair
both :: (a->b) -> (a,a) -> (b,b)
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
        ri = rowIndex r
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
        ri = columnIndex c
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


