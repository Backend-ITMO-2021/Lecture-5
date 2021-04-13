package ru.ifmo.backend_2021

object SudokuUtils {

  def getValuesFromArea(xIndices: List[Int], yIndices: List[Int])
                       (topLeftCorner: (Int, Int), sudoku: List[List[Int]]): List[Int] = {
    xIndices.flatMap(x => yIndices.map(y => sudoku(topLeftCorner._1 + y)(topLeftCorner._2 + x))).filter(_ != 0)
  }

  def areaValuesValid(area: ((Int, Int), List[List[Int]]) => List[Int])
                     (topLeftCorner: (Int, Int), sudoku: List[List[Int]]): Boolean = {
    val specifiedArea = area(topLeftCorner, sudoku)
    specifiedArea.distinct.length == specifiedArea.length
  }

  def getValuesFromHorizontal: ((Int, Int), List[List[Int]]) => List[Int] = {
    getValuesFromArea(Range(0, 9).toList, List(0))
  }

  def getValuesFromVertical: ((Int, Int), List[List[Int]]) => List[Int] = {
    getValuesFromArea(List(0), Range(0, 9).toList)
  }

  def getValuesFromSquare: ((Int, Int), List[List[Int]]) => List[Int] = {
    getValuesFromArea(List(0, 1, 2), List(0, 1, 2))
  }

  def isHorizontalValuesValid: ((Int, Int), List[List[Int]]) => Boolean = {
    areaValuesValid(getValuesFromHorizontal)
  }

  def isVerticalValuesValid: ((Int, Int), List[List[Int]]) => Boolean = {
    areaValuesValid(getValuesFromVertical)
  }

  def isSquareValuesValid: ((Int, Int), List[List[Int]]) => Boolean = {
    areaValuesValid(getValuesFromSquare)
  }

  def allHorizontalsValid(sudoku: List[List[Int]]): Boolean = {
    Range(0, 9).zipAll(List(0), 0, 0).forall(isHorizontalValuesValid(_, sudoku))
  }

  def allVerticalsValid(sudoku: List[List[Int]]): Boolean = {
    List(0).zipAll(Range(0, 9), 0, 0).forall(isVerticalValuesValid(_, sudoku))
  }

  def allSquaresValid(sudoku: List[List[Int]]): Boolean = {
    List(0, 3, 6).forall(x => List(0, 3, 6).forall(y => isSquareValuesValid((x, y), sudoku)))
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    allHorizontalsValid(rawSudoku) && allVerticalsValid(rawSudoku) && allSquaresValid(rawSudoku)
  }

  def renderSudokuLine(pair: (List[Int], String)): String = {
    val line = pair._1.map(num => if (num == 0) " " else num.toString).grouped(3).map(_.mkString(" ")).mkString(" | ")
    s"${pair._2} | $line |"
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val lineDelimiter: String = "\n--+-------+-------+-------+\n"
    val firstLine = renderSudokuLine(Range(1, 10).toList, " ")
    val otherLines = grid.zipWithIndex.map(pair => (pair._1, pair._2.toString))
      .grouped(3).map(_.map(renderSudokuLine).mkString("\n")).mkString(lineDelimiter)

    s"\n$firstLine$lineDelimiter$otherLines$lineDelimiter"
  }

  def validInput(move: (Int, Int, Int)): Boolean = {
    val range = Range(0, 9)
    range.contains(move._1) && range.contains(move._2) && range.contains(move._3 - 1)
  }

  def validMove(move: (Int, Int, Int), newSudoku: List[List[Int]], startPosition: List[List[Int]]): Boolean = {
    startPosition(move._1)(move._2) == 0 && isValidSudoku(newSudoku)
  } // оставим игроку право на ошибку, но запретим менять те ячейки, которые были объявлены в начале
}