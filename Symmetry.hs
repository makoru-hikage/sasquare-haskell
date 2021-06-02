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

both :: (a->b) -> (a,a) -> (b,b)
both f (x,y) = (f x, f y)

oddCenterIndex :: Int -> Int
oddCenterIndex x = ceiling (x % 2)

evenCenterIndices :: Int -> (Int,Int)
evenCenterIndices x = (mainCenterIndex, secondaryCellIndex)
    where
        mainCenterIndex = ceiling (x % 2)
        secondaryCellIndex = mainCenterIndex + 1

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

