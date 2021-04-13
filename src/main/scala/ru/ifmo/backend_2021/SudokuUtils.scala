package ru.ifmo.backend_2021

object SudokuUtils {

  def main(args: Array[String]): Unit = {
    val sudokuInput1 =
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

    val sudokuInput2 =
      List(
        List(3, 1, 6, 5, 7, 8, 4, 9, 2),
        List(5, 2, 9, 1, 3, 4, 7, 6, 8),
        List(4, 8, 7, 6, 2, 9, 5, 3, 1),

        List(2, 6, 3, 4, 1, 5, 9, 8, 7),
        List(9, 7, 4, 8, 6, 3, 1, 2, 5),
        List(8, 5, 1, 7, 9, 2, 6, 4, 3),

        List(1, 3, 8, 9, 4, 7, 2, 5, 6),
        List(6, 9, 2, 3, 5, 1, 8, 7, 4),
        List(7, 4, 5, 2, 8, 6, 3, 2, 9)
      )

    println(SudokuUtils.renderSudoku(sudokuInput1))

//    val game = new Game(sudokuInput2, new ComputerPlayer);
//    game.start()
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    !Range(0, 9).exists { i =>
      val row = Range(0, 9).map(rawSudoku(i)(_)).filter(_ > 0)
      val col = Range(0, 9).map(rawSudoku(_)(i)).filter(_ > 0)
      val square = Range(0, 9).map(j => rawSudoku((i / 3) * 3 + j / 3)((i % 3) * 3 + j % 3)).filter(_ > 0)
      row.distinct.length != row.length || col.distinct.length != col.length ||
        square.distinct.length != square.length
    }
  }

  def isFinishedSudoku(rawSudoku: List[List[Int]]): Boolean = {
    !Range(0, 9).exists { i =>
      val emptyCells = Range(0, 9).map(rawSudoku(i)(_)).filter(_ == 0)
      emptyCells.nonEmpty
    } && isValidSudoku(rawSudoku)
  }

  def renderSudoku(grid: List[List[Int]]) = {
    val firstString = s"  | ${Range(1, 10).map(i => if (i % 3 == 0) s"$i |" else i).mkString(" ")}"
    val horizontalBlockSeparator = firstString.replaceAll("[0-9 ]", "-").replaceAll("[|]", "+")
    grid.zipWithIndex.map(zip => {
      val row = zip._1
      val rowIndex = zip._2
      if (rowIndex == 0)
        s"\n$firstString\n${horizontalBlockSeparator}\n${renderRow(row, rowIndex)}"
      else if (rowIndex == grid.length - 1)
        s"${renderRow(row, rowIndex)}\n$horizontalBlockSeparator\n"
      else if (rowIndex % 3 == 2)
        s"${renderRow(row, rowIndex)}\n$horizontalBlockSeparator"
      else
        s"${renderRow(row, rowIndex)}"
    }).mkString("\n")
  }

  def renderRow(row: List[Int], rowIndex: Int): String = {
    row.zipWithIndex.map(zip => {
      val column = zip._2
      val value = if(zip._1 == 0) " " else zip._1
      if (column == 0)
        s"$rowIndex | $value"
      else if ( column % 3 == 2)
        s"$value |"
      else value
    }).mkString(" ")
  }
}
