package ru.ifmo.backend_2021


object SudokuUtils {
  val ROW_DELIMITER = "--+-------+-------+-------+"
  val SQUARE_DELIMITER = "|"


  def main(args: Array[String]): Unit = {
    new SudokuGame(List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 2),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),

      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),

      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )).start()
  }

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
    grid.flatMap(row => {
      row.zipWithIndex.map{case(cell, columnIndex) =>
        val rowIndex = grid.indexOf(row)
        if (rowIndex == 0 && columnIndex == 0) {
          s"\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n$ROW_DELIMITER\n0 $SQUARE_DELIMITER ${cellToString(cell)}"
        } else if (columnIndex == 0) {
          s"$rowIndex $SQUARE_DELIMITER ${cellToString(cell)}"
        }else if (columnIndex == row.length - 1 && rowIndex % 3 == 2) {
          s" ${cellToString(cell)} $SQUARE_DELIMITER\n$ROW_DELIMITER\n"
        }else if (columnIndex % 3 == 2 && columnIndex == row.length - 1 && rowIndex != grid.length - 1) {
          s" ${cellToString(cell)} $SQUARE_DELIMITER\n"
        } else if (columnIndex % 3 == 2 && columnIndex != row.length - 1) {
          s" ${cellToString(cell)} $SQUARE_DELIMITER"
        }  else if (columnIndex == row.length - 1 && rowIndex == grid.length - 1) {
          s" ${cellToString(cell)} $SQUARE_DELIMITER\n$ROW_DELIMITER\n"
        } else {
          s" ${cellToString(cell)}"
        }
      }
    }).mkString("")
  }

  private def cellToString(cell: Int) = if (cell == 0) " " else cell.toString

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