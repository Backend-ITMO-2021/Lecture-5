package ru.ifmo.backend_2021


object SudokuUtils {
  val EMPTY_VALUE = 0
  private val MIN_POS = 0
  private val MAX_POS = 8
  private val MIN_VALUE = 1
  private val MAX_VALUE = 9
  private val HEADER = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |"
  private val SEPARATOR = "\n--+-------+-------+-------+\n"
  private val DIVIDER = " | "
  private val LINE_END = " |"

  def isInputWrong(x: Int, y: Int, v: Int): Boolean = {
    x < MIN_POS || x > MAX_POS || y < MIN_POS || y > MAX_POS || v < MIN_VALUE || v > MAX_VALUE
  }

  def isValidSudoku(grid: List[List[Int]]): Boolean = {
    (0 to 8).foreach { i =>
      val row = (0 to 8).map(grid(i)(_)).filter(_ != 0)
      val column = (0 to 8).map(grid(_)(i)).filter(_ != 0)
      val square = (0 to 8).map(j => grid((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3)).filter(_ != 0)

      if (hasDuplicates(row) || hasDuplicates(column) || hasDuplicates(square)) {
        return false
      }
    }
    true
  }

  def isGameFinished(sudoku: List[List[Int]]): Boolean = sudoku.flatten.forall(_ > 0)

  def renderSudoku(grid: List[List[Int]]): String =  {
    HEADER + SEPARATOR + grid.indices.map(index => renderRow(index, grid(index)))
                             .sliding(3, 3).map { rs => rs.mkString("\n") }
                             .mkString(SEPARATOR) + SEPARATOR
  }

  private def renderRow(rowNumber: Int, row: List[Int]): String = {
    rowNumber + DIVIDER + row.map(x => if (x > 0) x.toString else " ")
                             .sliding(3, 3).map { c => c.mkString(" ") }
                             .mkString(DIVIDER) + LINE_END
  }

  private def hasDuplicates(list: IndexedSeq[Int]): Boolean = (list.toSet.size != list.size)
}