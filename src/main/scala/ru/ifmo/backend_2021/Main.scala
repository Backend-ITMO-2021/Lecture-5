package ru.ifmo.backend_2021

object Main {
  def main(args: Array[String]): Unit = {
    lazy val correctSudoku =
      List(
        List(3, 1, 6, 5, 7, 8, 4, 9, 2),
        List(5, 2, 9, 1, 3, 4, 7, 6, 8),
        List(4, 8, 7, 6, 2, 9, 5, 3, 1),

        List(2, 6, 3, 4, 1, 5, 9, 8, 7),
        List(9, 7, 4, 8, 6, 3, 1, 2, 5),
        List(8, 5, 1, 7, 9, 2, 6, 4, 3),

        List(1, 3, 8, 9, 4, 7, 2, 5, 6),
        List(6, 9, 2, 3, 5, 1, 8, 7, 4),
        List(7, 4, 5, 2, 8, 6, 3, 1, 0)
      )
    SudokuGame.run(correctSudoku)
  }
}
