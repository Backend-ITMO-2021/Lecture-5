package ru.ifmo.backend_2021

object SudokuUtils {
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
    )).start()
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    !Range(0, 9).exists { i =>
      val row = Range(0, 9).map(rawSudoku(i)(_)).filter(_ != 0)
      val col = Range(0, 9).map(rawSudoku(_)(i)).filter(_ != 0)
      val square = Range(0, 9).map(j => rawSudoku((i % 3) * 3 + j % 3)((i / 3) * 3 + j / 3)).filter(_ != 0)
      row.distinct.length != row.length || col.distinct.length != col.length || square.distinct.length != square.length
    }
  }

  def renderSudoku(grid: List[List[Int]], userValue: Int = -1) = {
    val header = s"  | 1 2 3 | 4 5 6 | 7 8 9 |"
    val separator = s"\n--+-------+-------+-------+"
    Range(0, 9).map(i =>
      if(i == 0) s"\n$header$separator\n$i | " + renderRow(grid, i, userValue)
      else if (i == 8) s"$i | " + renderRow(grid, i, userValue) + s"$separator\n"
      else if (i % 3 == 2) s"$i | " + renderRow(grid, i, userValue) + s"$separator"
      else s"$i | " + renderRow(grid, i, userValue)
    ).mkString("\n")
  }

  def renderRow(grid: List[List[Int]], i: Int, userValue: Int) = {
    Range(0, 9).map(j =>
      if (j % 3 == 2) nullsAndErrorsCheck(grid(i)(j), userValue) + " |" else nullsAndErrorsCheck(grid(i)(j), userValue)
    ).mkString(" ")
  }

  def nullsAndErrorsCheck(num: Int, userValue: Int) = {
    val roundedNumber = List("①", "②", "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨")
    if (num == 0) " "
    else if (num == -1) roundedNumber(userValue - 1)
    else num
  }
}