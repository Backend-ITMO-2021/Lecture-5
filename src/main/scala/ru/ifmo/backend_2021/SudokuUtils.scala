package ru.ifmo.backend_2021


object SudokuUtils {
  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    for (i <- Range(0, 9)) {
      if (!isValidRow(rawSudoku, i)) {
        return false
      }
      if (!isValidColumn(rawSudoku, i)) {
        return false
      }
      if (!isValidSquare(rawSudoku.slice(i / 3 * 3, i / 3 * 3 + 3).transpose.slice(i % 3 * 3, i % 3 * 3 + 3))) {
        return false
      }
    }

    true
  }

  def isValidSequence(sequence: List[Int]): Boolean = {
    if (sequence.count(_ == 0) > 0) {
      sequence.distinct.length == sequence.count(_ > 0) + 1
    } else {
      sequence.distinct.length == 9
    }
  }

  private def isValidRow(grid: List[List[Int]], rowIndex: Int): Boolean = isValidSequence(grid(rowIndex))
  private def isValidColumn(grid: List[List[Int]], columnIndex: Int): Boolean = {
    val transformedGrid = grid.transpose
    isValidSequence(transformedGrid(columnIndex))
  }
  private def isValidSquare(square: List[List[Int]]): Boolean = isValidSequence(square.flatten)

  private def renderCell(row: List[Int], cellIndex: Int, rowBuffer: String): String = {
    val cellDivider = "| "

    if (cellIndex < 9) {
      s"${if (cellIndex % 3 == 0) cellDivider else ""}" +
        s"${if (row(cellIndex) != 0) row(cellIndex).toString else " "}" +
        " " + renderCell(row, cellIndex + 1, rowBuffer)
    } else {
      rowBuffer
    }
  }

  private def renderRow(grid: List[List[Int]], rowIndex: Int, buffer: String): String = {
    val rowDivider = "--+-------+-------+-------+\n"
    if (rowIndex < 9) {
      s"${if (rowIndex % 3 == 0) rowDivider else "" }" +
        rowIndex.toString + " " + renderCell(grid(rowIndex), 0, "") +
        "|\n" + renderRow(grid, rowIndex + 1, buffer)
    } else {
      buffer + rowDivider
    }
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val firstLine = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n"

    firstLine + renderRow(grid, 0, "")
  }
}