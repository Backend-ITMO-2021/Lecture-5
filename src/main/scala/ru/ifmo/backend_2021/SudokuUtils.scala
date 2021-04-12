package ru.ifmo.backend_2021


object SudokuUtils {
  val DELIMITER_ROW = "--+-------+-------+-------+"
  val DELIMITER_COL = " |"


  // FUNCTIONS FROM TASK

  // Checks overall field validity
  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    val transposedField = rawSudoku.transpose

    for (i <- rawSudoku.indices) {
      for (j <- rawSudoku(i).indices) {
        val row = rawSudoku(i)
        val col = transposedField(j)
        val smallField = getSmallField(rawSudoku, i, j)
        if (!isUniqueValues(row) || !isUniqueValues(col) || !isUniqueValues(smallField)) {
          return false
        }
      }
    }

    true
  }

  // Renders sudoku field
  def renderSudoku(grid: List[List[Int]], highlightedCells: List[(Int, Int)] = null, i: Int = -1, j: Int = -1, newRow: Boolean = false): String = {
    if (i == -1 && j == -1) {
      s"\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n" +
      s"${renderSudoku(grid, highlightedCells, 0, 0, true)}"

    } else {
      if (newRow) {
        if (i == 9) {
          s"$DELIMITER_ROW\n"
        } else {
          s"${if (i % 3 == 0) DELIMITER_ROW + "\n" else ""}" +
          s"$i${renderSudoku(grid, highlightedCells, i, j)}"
        }

      } else {
        s"${if (j % 3 == 0) DELIMITER_COL else ""}" +
        s"${if (highlightedCells != null && highlightedCells.contains((i, j))) ">" else " "}" +
        s"${if (grid(i)(j) == 0) " " else grid(i)(j)}" +
        s"${if (j == 8) DELIMITER_COL + "\n" else ""}" +
        s"${renderSudoku(grid, highlightedCells, i + ((j + 1) / 9), (j + 1) % 9, j == 8)}"
      }
    }
  }


  // ADDITIONAL FUNCTIONS

  // Returns tuple with coordinates of duplicated value from current row, column or 3x3 field
  def getDuplicationCoordinates(rawSudoku: List[List[Int]], i: Int, j: Int, v: Int): (Int, Int) = {
    val transposedField = rawSudoku.transpose

    val row = rawSudoku(i)
    val col = transposedField(j)
    val smallField = getSmallField(rawSudoku, i, j)

    if (row.contains(v)) {
      return (i, row.indexOf(v))
    }

    if (col.contains(v)) {
      return (col.indexOf(v), j)
    }

    if (smallField.contains(v)) {
      val startX = getSmallFieldStart(i)
      val startY = getSmallFieldStart(j)
      val position = smallField.indexOf(v)
      return (startX + position / 3, startY + position % 3)
    }

    (-1, -1)
  }

  // Returns values from current 3x3
  private def getSmallField(rawSudoku: List[List[Int]], i: Int, j: Int): List[Int] = {
    val startX = getSmallFieldStart(i)
    val startY = getSmallFieldStart(j)

    rawSudoku
      .slice(startX, startX + 3)
      .transpose
      .slice(startY, startY + 3)
      .flatten
  }

  // Calculates of 3x3 field start index
  private def getSmallFieldStart(index: Int): Int = {
    (index / 3) * 3
  }

  // Checks is all the values are unique (without zeros)
  private def isUniqueValues(values: List[Int]): Boolean = {
    val withoutZeros = values.filter(v => v != 0)
    withoutZeros.distinct.size == withoutZeros.size
  }
}