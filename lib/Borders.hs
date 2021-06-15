module Borders (
    Region(..),
    topLeftCorner,
    topRightCorner,
    bottomLeftCorner,
    bottomRightCorner,
    topEdge,
    bottomEdge,
    leftEdge,
    rightEdge,
    allCorners,
    allEdges,
    regionOfCell
) where

import Data.Ratio ( (%) )
import Data.List ( nub )
import Data.Maybe ( mapMaybe )

import BasicParts (
    Base,
    Row(..),
    Column(..),
    Cell(..),
    CellSet (..),
    cellIndex,
    cellIndices,
    getRow,
    getColumn,
    getIndex,
    getBase,
    isCellValid,
    isRowValid,
    isColumnValid,
    findCellByIndices,
    nthCellOfRow,
    nthCellOfColumn
    )

data Region =
    TopEdge
    | BottomEdge
    | LeftEdge
    | RightEdge
    | TopLeftCorner
    | TopRightCorner
    | BottomLeftCorner
    | BottomRightCorner
    | Middle
    | Outside
    deriving (Show)

instance Eq Region where
    TopEdge == TopEdge = True
    BottomEdge == BottomEdge = True
    LeftEdge == LeftEdge = True
    RightEdge == RightEdge = True
    TopLeftCorner == TopLeftCorner = True
    TopRightCorner == TopRightCorner = True
    BottomLeftCorner == BottomLeftCorner = True
    BottomRightCorner == BottomRightCorner = True
    Middle == Middle = True
    Outside == Outside = True
    _ == _ = False

-- Borders
topLeftCorner :: Base -> Cell
topLeftCorner b = Cell b 1

topRightCorner :: Base -> Cell
topRightCorner b = Cell b b

bottomLeftCorner :: Base -> Cell
bottomLeftCorner b = Cell b (b^2 - b + 1)

bottomRightCorner :: Base -> Cell
bottomRightCorner b = Cell b (b^2)

topEdge :: Base -> Row
topEdge b = Row b 1

bottomEdge :: Base -> Row
bottomEdge b = Row b b

leftEdge :: Base -> Column
leftEdge b = Column b 1

rightEdge :: Base -> Column
rightEdge b = Column b b

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
allEdges b = nub (horizontalEdges ++ verticalEdges)
    where
        verticalEdges = concatMap (getCellsOf . ($b)) [leftEdge,rightEdge]
        horizontalEdges = concatMap (getCellsOf . ($b)) [topEdge,bottomEdge]

regionOfCell :: Cell -> Region
regionOfCell i
    | not (isCellValid i) = Outside
    | i == topLeftCorner b = TopLeftCorner
    | i == topRightCorner b = TopRightCorner
    | i == bottomLeftCorner b = BottomLeftCorner
    | i == bottomRightCorner b = BottomRightCorner
    | isCellIn i (topEdge b) = TopEdge
    | isCellIn i (bottomEdge b) = BottomEdge
    | isCellIn i (leftEdge b) = LeftEdge
    | isCellIn i (rightEdge b) = RightEdge
    | otherwise = Middle
    where b = getBase i

isInside :: Cell -> Bool
isInside i = regionOfCell i /= Outside

isInBorder :: Cell -> Bool
isInBorder i = isInside i && regionOfCell i /= Middle