package ru.ifmo.backend_2021


object SudokuUtils {
  def isValidSudoku(grid: List[List[Int]]): Boolean = {
    val boxes = grid.zipWithIndex.map { case (x, i) =>
      x.zipWithIndex.map { case (_, j) =>
        grid((i / 3) * 3 + j / 3)((i % 3) * 3 + j % 3)
      }
    }
    grid.foreach(row => if (checkValid(row)) return false)
    grid.transpose.foreach(col => if (checkValid(col)) return false)
    boxes.foreach(box => if (checkValid(box)) return false)
    true
  }

  private def checkValid(list: List[Int]): Boolean = list.filter(x => x != 0).groupBy(x => x).map(_._2.size).exists(x => x > 1)

  def renderSudokuLine(pair: (List[Int], String)): String = {
    val line = pair._1.map(num => if (num == 0) " " else num.toString).grouped(3).map(_.mkString(" ")).mkString(" | ")
    s"${pair._2} | $line |"
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val sep = "\n--+-------+-------+-------+\n"
    val header = " | 1 2 3 | 4 5 6 | 7 8 9 |"
    val otherLines = grid.zipWithIndex.map(pair => (pair._1, pair._2.toString))
      .grouped(3).map(_.map(renderSudokuLine).mkString("\n")).mkString(sep)

    s"\n$header$sep$otherLines$sep"
  }

  def checkWin(grid: List[List[Int]]): Boolean = {
    grid.foreach(row => {
      if (row.contains(0)) return false
    })
    true
  }
}