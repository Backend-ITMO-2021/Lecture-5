package ru.ifmo.backend_2021

object SudokuUtils {
  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    for (curStep <- Range(0, 9))
        if (!isValidRow(rawSudoku, curStep) || !isValidCol(rawSudoku, curStep)
          || !isValidSquare(rawSudoku, curStep / 3 * 3, curStep % 3 * 3))
          return false
    true
  }

  def renderSudoku(grid: List[List[Int]], incorrectPos: List[(Int, Int)] = null): String = {
    grid.flatMap(rowList => {
      rowList.zipWithIndex.map{case(number, col) =>
        val row = grid.indexOf(rowList)
        val startStr = if (row == 0 && col == 0) "\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n--+-------+-------+-------+\n0 | " else
          if (col == 0) s"$row | " else " "
        val numberStr =  if (number == 0) " " else
          if (incorrectPos != null && incorrectPos.contains((row, col)))
            Console.RED + number.toString + Console.RESET else number.toString
        val endStr = if (col == rowList.length - 1 && row % 3 == 2) " |\n--+-------+-------+-------+\n" else
          if (col % 3 == 2 && col == rowList.length - 1 && row != grid.length - 1) " |\n" else
            if (col % 3 == 2 && col != rowList.length - 1) " |" else
              if (col == rowList.length - 1 && row == grid.length - 1) " |\n--+-------+-------+-------+\n" else ""
        startStr + numberStr + endStr
      }
    }).mkString("")
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
    val incorrectPos = List((row, col))
    val incorrectRowPos = getIncorrectRowPos(grid, row, col, value)
    val incorrectColPos = getIncorrectColPos(grid, row, col, value)
    val incorrectSquarePos = getIncorrectSquarePos(grid, row, col, value)
    incorrectPos ++ incorrectRowPos ++ incorrectColPos ++ incorrectSquarePos
  }

  def getIncorrectRowPos(grid: List[List[Int]], row: Int, col:Int, value:Int): List[(Int, Int)] = {
    if (!isValidRow(grid, row)) Range(0, 9).foreach(i =>
      if (grid(row)(i) == value && i != col)
        return List((row, i)))
    List((0,0)).empty
  }

  def getIncorrectColPos(grid: List[List[Int]], row: Int, col:Int, value:Int): List[(Int, Int)] = {
    if (!isValidCol(grid, col)) {
      val curGrid = grid.transpose
      Range(0, 9).foreach(i => if (curGrid(col)(i) == value && i != row) return List((i, col)))
    }
    List((0,0)).empty
  }

  def getIncorrectSquarePos(grid: List[List[Int]], row: Int, col:Int, value:Int): List[(Int, Int)] = {
    if (!isValidSquare(grid, row, col)) {
      val startRow = row / 3 * 3
      val startCol = col / 3 * 3
      val curSquare = grid.slice(startRow, startRow + 3).transpose.slice(startCol, startCol + 3).transpose
      Range(0, 3).foreach(i => Range(0, 3).foreach(j =>
        if (curSquare(i)(j) == value && startRow + i != row && startCol + j != col)
          return List((startRow + i, startCol + j))))
    }
    List((0,0)).empty
  }
}