import Data.Ratio ( (%) )
import Data.List ( nub )
import Data.Maybe ( mapMaybe, isJust, fromJust, catMaybes )

import BasicParts (
    Base,
    Row(..),
    Column(..),
    Cell(..),
    cellIndex,
    cellIndices,
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
    nthCellOfColumn,
    columnCells
    )

-- Find the centermost element in a finite list of an odd length
oddCenterIndex :: Int -> Int
oddCenterIndex x = ceiling (x % 2)

-- Find the pair of centermost elements 
-- in a finite list of an even length
evenCenterIndices :: Int -> [Int]
evenCenterIndices x = [mainCenterIndex, secondaryCellIndex]
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
rowCenter :: Row -> [Cell]
rowCenter r
    | isRowValid r && odd b = oddCenter
    | isRowValid r && even b = evenCenter
    | otherwise = []
    where
        b = getBase r
        ri = getIndex r
        oddCenter = mapMaybe (findCellByIndices b ri) [oddCenterIndex b]
        evenCenter = mapMaybe (nthCellOfRow r) (evenCenterIndices b)

-- Find the center cell of a column
columnCenter :: Column -> [Cell]
columnCenter c
    | isColumnValid c && odd b = oddCenter
    | isColumnValid c && even b = evenCenter
    | otherwise = []
    where
        b = getBase c
        ri = getIndex c
        oddCenter = mapMaybe (findCellByIndices b ri) [oddCenterIndex b]
        evenCenter = mapMaybe (nthCellOfColumn c) (evenCenterIndices b)

-- Find the center cell of a square if the base is odd
-- Find the four center cells of a square if the base is even
squareCenter :: Base -> [Cell]
squareCenter b
    | b < 1 = []
    | odd b = oddCenter
    | even b = map (Cell b) evenCenterCellIndices
    where
        m = oddCenterIndex b
        oddCenter =
            catMaybes [findCellByIndices b m m] 
        oddCenterCellIndex = cellIndex $ head oddCenter
        evenCenterCellIndices = [
            oddCenterCellIndex,
            oddCenterCellIndex + 1,
            oddCenterCellIndex + b,
            oddCenterCellIndex + b + 1
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

-- Borders
topLeftCorner :: Base -> Cell
topLeftCorner b = Cell b 1

topRightCorner :: Base -> Cell
topRightCorner b = Cell b b

bottomLeftCorner :: Base -> Cell
bottomLeftCorner b = Cell b (b^2 - b + 1)

bottomRightCorner :: Base -> Cell
bottomRightCorner b = Cell b (b^2)

topEdge :: Base -> [Cell]
topEdge b = rowCells $ Row b 1

bottomEdge :: Base -> [Cell]
bottomEdge b = rowCells $ Row b b

leftEdge :: Base -> [Cell]
leftEdge b = columnCells $ Column b 1

rightEdge :: Base -> [Cell]
rightEdge b = columnCells $ Column b b

allCorners :: Base -> [Cell]
allCorners b = map ($b) corners
    where
        corners = [
            topLeftCorner,
            topRightCorner,
            bottomLeftCorner,
            bottomRightCorner
            ]

allEdges :: Base -> [Cell]
allEdges b = (nub . concat) edges
    where edges = map ($b) [topEdge,leftEdge,rightEdge,bottomEdge]
