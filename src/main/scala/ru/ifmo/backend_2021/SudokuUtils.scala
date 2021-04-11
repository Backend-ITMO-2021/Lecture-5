package ru.ifmo.backend_2021


object SudokuUtils {
  val ROW_DELIMITER = "--+-------+-------+-------+"
  val SQUARE_DELIMITER = " |"

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    val columns = rawSudoku.transpose
    val squares = getSquares(rawSudoku)
    for (i <- rawSudoku.indices) {
      val row = rawSudoku(i)
      for (j <- rawSudoku(i).indices) {
        if (!isCellValid(rawSudoku(i)(j), row, columns(j), squares((i / 3) * 3 + j / 3))) {
          return false
        }
      }
    }
    true
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val sb = new StringBuilder()
    sb.append(s"\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n")
    for (i <- grid.indices) {
      if (i % 3 == 0) {
        sb.append(s"$ROW_DELIMITER\n")
      }
      sb.append(s"$i")
      for (j <- grid(i).indices) {
        if (j % 3 == 0) {
          sb.append(s"$SQUARE_DELIMITER")
        }
        val el = grid(i)(j)
        sb.append(s" ${if (el == 0) " " else el}")
      }
      sb.append(s"$SQUARE_DELIMITER\n")
    }
    sb.append(s"$ROW_DELIMITER\n")
    sb.toString()
  }

  def isCellValid(number: Int, row: List[Int], column: List[Int], square: List[Int]): Boolean = {
    number == 0 || column.count(el => {
      number == el
    }) == 1 && row.count(el => {
      number == el
    }) == 1 && square.count(el => {
      number == el
    }) == 1
  }

  def getSquares(rawSudoku: List[List[Int]]): List[List[Int]] = List[List[Int]](
    getSquare(rawSudoku, 0, 0), getSquare(rawSudoku, 0, 3), getSquare(rawSudoku, 0, 6),
    getSquare(rawSudoku, 3, 0), getSquare(rawSudoku, 3, 3), getSquare(rawSudoku, 3, 6),
    getSquare(rawSudoku, 6, 0), getSquare(rawSudoku, 6, 3), getSquare(rawSudoku, 6, 6),
  )

  def getSquare(rawSudoku: List[List[Int]], i: Int, j: Int): List[Int] =
    rawSudoku
      .slice((i / 3) * 3, (i / 3) * 3 + 3)
      .transpose
      .slice((j / 3) * 3, (j / 3) * 3 + 3)
      .flatten
}