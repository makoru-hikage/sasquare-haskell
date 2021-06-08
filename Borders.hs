import Data.Ratio ( (%) )
import Data.List ( nub )
import Data.Maybe ( mapMaybe )

import BasicParts (
    Base,
    Row(..),
    Column(..),
    Cell(..),
    CellSet (getCellsOf),
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

data Edge = TopEdge | BottomEdge | LeftEdge | RightEdge

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
 