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
      return sequence.distinct.length == sequence.count(_ > 0) + 1
    } else {
      return sequence.distinct.length == 9
    }
  }

  private def isValidRow(grid: List[List[Int]], rowIndex: Int): Boolean = isValidSequence(grid(rowIndex))
  private def isValidColumn(grid: List[List[Int]], columnIndex: Int): Boolean = {
    val transformedGrid = grid.transpose
    isValidSequence(transformedGrid(columnIndex))
  }
  private def isValidSquare(square: List[List[Int]]): Boolean = isValidSequence(square.flatten)

  def renderSudoku(grid: List[List[Int]]) = {
    val sb = new StringBuilder()
    val firstLine = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n"
    val rowDivider = "--+-------+-------+-------+\n"
    val cellDivider = " | "
    sb.append(firstLine);
    for (i <- grid.indices) {
      if (i % 3 == 0) {
        sb.append(rowDivider)
      }
      sb.append(i.toString)
      for (j <- grid(i).indices) {
        if (j % 3 == 0) {
          sb.append(cellDivider)
        }
        if (grid(i)(j) == 0) {
          sb.append(" ")
        } else {
          sb.append(grid(i)(j).toString)
        }
        if (j % 3 != 2) {
          sb.append(" ")
        }
      }
      sb.append(" |\n")
    }
    sb.append(rowDivider)
    sb.toString()
  }
}