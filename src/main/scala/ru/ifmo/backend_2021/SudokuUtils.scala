package ru.ifmo.backend_2021


object SudokuUtils {
  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    (0 until 9).toList.foreach(index => {
      val rowValues = rawSudoku(index)
        .filter(_ != 0)
      val columnValues = rawSudoku
        .map(row => row(index))
        .filter(_ != 0)
      if (rowValues.length != rowValues.distinct.length || columnValues.length != columnValues.distinct.length) {
        return false
      }

      if (index % 3 == 0) {
        val slicedRow1 = splitRowValues(rawSudoku(index))
        val slicedRow2 = splitRowValues(rawSudoku(index + 1))
        val slicedRow3 = splitRowValues(rawSudoku(index + 2))

        val filtered1 = (slicedRow1._1 ++ slicedRow2._1 ++ slicedRow3._1).filter(_ != 0)
        val filtered2 = (slicedRow1._2 ++ slicedRow2._2 ++ slicedRow3._2).filter(_ != 0)
        val filtered3 = (slicedRow1._3 ++ slicedRow2._3 ++ slicedRow3._3).filter(_ != 0)

        if (filtered1.length != filtered1.distinct.length || filtered2.length != filtered2.distinct.length || filtered3.length != filtered3.distinct.length) {
          return false
        }
      }
    })
    true
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val firstLine = "  | 1 2 3 | 4 5 6 | 7 8 9 |"
    val rowSeparator = "--+-------+-------+-------+"

    grid.zipWithIndex.map {
      case (row, index) => {
        index match {
          case index if index == 0 => s"\n$firstLine\n$rowSeparator\n${printRow(index, row)}"
          case index if (index + 1) % 3 == 0 =>
            s"${printRow(index, row)}$rowSeparator\n"
          case _ => printRow(index, row)
        }
      }
    }.mkString("")

  }

  private def splitRowValues(row: List[Int]): (List[Int], List[Int], List[Int]) = {
    (List(row(0), row(1), row(2)), List(row(3), row(4), row(5)), List(row(6), row(7), row(8)))
  }

  private def printRow(index: Int, row: List[Int]): String = {
    val newRow = row.map(value =>
      if (value == 0) " " else value.toString
    )
    s"$index | ${newRow(0)} ${newRow(1)} ${newRow(2)} | ${newRow(3)} ${newRow(4)} ${newRow(5)} | ${newRow(6)} ${newRow(7)} ${newRow(8)} |\n"
  }

  def updateSudoku(sudoku: List[List[Int]], x: Int, y: Int, v: Int): List[List[Int]] = {
    sudoku.updated(x, sudoku(x).updated(y, v))
  }
}