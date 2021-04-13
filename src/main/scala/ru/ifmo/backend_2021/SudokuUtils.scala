package ru.ifmo.backend_2021


object SudokuUtils {
  val ROW = "--+-------+-------+-------+"

  def main(args: Array[String]): Unit = {
    new Game(List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 2),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),

      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),

      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )).run()
  }
  def renderSudoku(grid: List[List[Int]]): String = {
    var sudokuString = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n"
    for (row <- Range(0, 9)) {
      if (row % 3 == 0)
        sudokuString += s"$ROW\n"
      sudokuString += row.toString + " "
      for (col <- Range(0, 9)) {
        if (col % 3 == 0)
          sudokuString += "| "
        if (grid(row)(col) != 0) {
          sudokuString += grid(row)(col).toString + " "
        } else
          sudokuString += "  "
      }
      sudokuString += "|\n"
    }
    sudokuString += s"$ROW\n"
    sudokuString
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    for (x <- Range(0, 9)) {
      val row = Range(0, 9).map(rawSudoku(x)(_)).filter(_ > 0)
      val col = Range(0, 9).map(rawSudoku(_)(x)).filter(_ > 0)
      val square = Range(0, 9).map(j => rawSudoku(x / 3 * 3 + j / 3)(x % 3 * 3 + j % 3)).filter(_ > 0)
      if (row.distinct.length != row.length ||
        col.distinct.length != col.length ||
        square.distinct.length != square.length) {
        return false
      }
    }
    true
  }
}