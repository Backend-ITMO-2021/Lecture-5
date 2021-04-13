package ru.ifmo.backend_2021

object Test {
  def main(args: Array[String]): Unit = {
    val correctSudoku =
      List(
        List(3, 1, 6, 5, 7, 8, 4, 9, 2),
        List(5, 2, 9, 1, 3, 4, 7, 6, 8),
        List(4, 8, 7, 6, 2, 9, 5, 3, 1),

        List(2, 6, 3, 0, 1, 0, 0, 8, 0),
        List(9, 7, 4, 8, 6, 3, 0, 0, 5),
        List(8, 5, 1, 0, 9, 0, 6, 0, 0),

        List(1, 3, 0, 0, 0, 0, 2, 5, 0),
        List(0, 0, 0, 0, 0, 0, 0, 7, 4),
        List(0, 0, 5, 2, 0, 6, 3, 0, 0)
      )

    val sudoku = new Sudoku(correctSudoku)
    sudoku.play()

//    val s = List(3, 5, 4, 2, 9, 8, 1, 0, 0)
//    val s2 = List(1, 2, 8, 6, 7, 5, 3, 0, 0)
//    println(SudokuUtils.isValidSequence(s2))
  }
}
