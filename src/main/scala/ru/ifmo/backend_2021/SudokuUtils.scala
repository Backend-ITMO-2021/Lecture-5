package ru.ifmo.backend_2021

object SudokuUtils {
  val ROW_DELIMITER = "--+-------+-------+-------+\n"
  val COL_DELIMITER = " | "
  val HEAD = "  | 1 2 3 | 4 5 6 | 7 8 9 |\n"

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    for (i <- 0 to 8) {
      for (j <- 0 to 8) {
        if (!(isSuitable(rawSudoku, i, j))) {
        return false}
        
      }
    }
    true
  }

  def isUniqueInRow(rawSudoku: List[List[Int]], rowNum: Integer, colNum: Int, element: Int): (Boolean, (Int, Int)) = {
    for (number <- rawSudoku(rowNum)) if (element == number && colNum != rawSudoku(rowNum).indexOf(number)) {
        return (false, (rawSudoku(rowNum).indexOf(number), rowNum))}
    true
  }

  def isUniqueInCol(rawSudoku: List[List[Int]], rowNum: Int, colNum: Integer, element: Int): (Boolean, (Int, Int)) = {
    for (row <- rawSudoku) if (element == row(colNum) && rowNum != rawSudoku.indexOf(row)) {
        return (false, (colNum, rawSudoku.indexOf(row)))}
    true
  }

  def isUniqueInCell(rawSudoku: List[List[Int]], rowNum: Int, colNum: Int, element: Int): (Boolean, (Int, Int)) = {
    val colStart = (colNum / 3) * 3
    val rowStart = (rowNum / 3) * 3
    val colEnd = colStart + 2
    val rowEnd = rowStart + 2
    for (i <- rowStart to rowEnd) {
      for (j <- colStart to colEnd) {
        if (element == rawSudoku(i)(j) && i != rowNum && j != colNum) {
        return (false, (j, i))}
      }
    }
    true
  }

  def isSuitable(rawSudoku: List[List[Int]], rowNum: Int, colNum: Int): (Boolean, (Int, Int), String) = {
    val element = rawSudoku(rowNum)(colNum)
    if (rawSudoku(rowNum)(colNum) == 0) return true
    if (!(isUniqueInCell(rawSudoku, rowNum, colNum, element) && isUniqueInCol(rawSudoku, rowNum, colNum, element) && isUniqueInRow(rawSudoku, rowNum, colNum, element))) return false
    true
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    "\n" + HEAD + ROW_DELIMITER + grid.zipWithIndex.map(idVal => renderRow(idVal._1, idVal._2)).mkString("")
  }

  def renderRow(row: List[Int], rowNum: Int): String = {
    val rowSplt = row.grouped(3).toList
    rowNum.toString + COL_DELIMITER + rowSplt.map(rowSngl => {rowSngl.mkString(" ").replace('0', ' ') + COL_DELIMITER}).mkString("").dropRight(1) + "\n" + (if ((rowNum + 1) % 3 == 0) ROW_DELIMITER else "")
  }
}