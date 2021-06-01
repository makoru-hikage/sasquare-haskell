import Parts

-- An equation used to find the ascending slant of a cell
intersectionSum :: Cell -> Int
intersectionSum i = rowIndex (getRow i) + columnIndex (getColumn i)

-- An equation used to find the descending slant of a cell
intersectionDiff :: Cell -> Int
intersectionDiff i = columnIndex (getColumn i) - rowIndex (getRow i)

rIntersectionDiff :: Cell -> Int
rIntersectionDiff i = rowIndex (getRow i) - columnIndex (getColumn i)