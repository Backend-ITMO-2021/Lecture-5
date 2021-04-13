package ru.ifmo.backend_2021


object SudokuUtils {

  val sep = "\n--+-------+-------+-------+\n"
  val header = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |"

  def isFilledSudoku(rawSudoku: List[List[Int]]): Boolean =
    !rawSudoku.flatMap { l => l }.contains(0)

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = !Range(0, 9).exists { i => List(
    rawSudoku(i),
    Range(0, 9).map(rawSudoku(_)(i)),
    Range((i / 3) * 3, (i / 3) * 3 + 3).map(rawSudoku)
      .flatMap { r => Range((i % 3) * 3, (i % 3) * 3 + 3).map(r) }
  ).map(l => l.filter(_ != 0)).exists(l => l.distinct.length != l.length) }

  def renderSudoku(grid: List[List[Int]]): String = header + sep +
    grid.zipWithIndex.map { case (r, y) => renderSudokuLine(y, r) }
      .sliding(3, 3).map { rs => rs.mkString("\n") }
      .mkString(sep) + sep

  def renderSudokuLine(n: Int, line: List[Int]): String = n + " | " +
    line.map(renderSudokuNumber)
      .sliding(3, 3).map { c => c.mkString(" ") }
      .mkString(" | ") + " |"

  def renderSudokuNumber(n: Int): String =
    if (n > 0) n.toString else " "
}
