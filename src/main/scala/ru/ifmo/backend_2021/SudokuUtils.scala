package ru.ifmo.backend_2021
import scala.util.control.Breaks._

object SudokuUtils {
  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    var result = true
    for (curStep <- Range(0, 9))
        if (!isValidRow(rawSudoku, curStep) || !isValidCol(rawSudoku, curStep)
          || !isValidSquare(rawSudoku, curStep / 3 * 3, curStep % 3 * 3))
          result = false
    result
  }

  def renderSudoku(grid: List[List[Int]], incorrectPos: List[(Int, Int)] = null): String = {
    var sudokuString = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n"
    for (curRow <- Range(0, 9)) {
      if (curRow % 3 == 0)
        sudokuString += "--+-------+-------+-------+\n"
      sudokuString += curRow.toString + " "
      for (curCol <- Range(0, 9)) {
        if (curCol % 3 == 0)
          sudokuString += "| "
        if (grid(curRow)(curCol) != 0) {
          if (incorrectPos != null && incorrectPos.contains((curRow, curCol)))
            sudokuString += Console.RED + grid(curRow)(curCol).toString + Console.RESET + " "
          else
            sudokuString += grid(curRow)(curCol).toString + " "
        } else
          sudokuString += "  "
      }
      sudokuString += "|\n"
    }
    sudokuString += "--+-------+-------+-------+\n"
    sudokuString
  }

  private def isValidRow(grid: List[List[Int]], row: Int): Boolean = {
    val curRow = grid(row).filter(_ > 0)
    curRow.size == curRow.distinct.size
  }

  private def isValidCol(grid: List[List[Int]], col: Int): Boolean = {
    val curGrid = grid.transpose
    val curCol = curGrid(col).filter(_ > 0)
    curCol.size == curCol.distinct.size
  }

  private def isValidSquare(grid: List[List[Int]], row: Int, col:Int): Boolean = {
    val startRow = row / 3 * 3
    val startCol = col /3 * 3
    val curSquare = grid.slice(startRow, startRow + 3).transpose.slice(startCol, startCol + 3).flatten.filter(_ > 0)
    curSquare.size == curSquare.distinct.size
  }

  def incorrectStep(grid: List[List[Int]], row: Int, col:Int, value:Int): List[(Int, Int)] = {
    var incorrectPos = List((row, col))
    if (!isValidRow(grid, row)) {
      Range(0, 9).foreach(i =>
        if (grid(row)(i) == value && i != col) incorrectPos = incorrectPos :+ (row, i))
    }
    if (!isValidCol(grid, col)) {
      val curGrid = grid.transpose
      Range(0, 9).foreach(i =>
        if (curGrid(col)(i) == value && i != row) incorrectPos = incorrectPos :+ (i, col))
    }
    if (!isValidSquare(grid, row, col)) {
      val startRow = row / 3 * 3
      val startCol = col / 3 * 3
      val curSquare = grid.slice(startRow, startRow + 3).transpose.slice(startCol, startCol + 3).transpose
      Range(0, 3).foreach(i => Range(0, 3).foreach(j =>
        if (curSquare(i)(j) == value && startRow + i != row && startCol + j != col) incorrectPos = incorrectPos :+ (startRow + i, startCol + j)))
    }
    incorrectPos
  }
}