package ru.ifmo.backend_2021

import scala.collection.mutable.ArrayBuffer

/**
 * @author Vladimir Goncharov
 * @created 11.04.2021
 */
class ComputerPlayer extends Player {
  private val route = ArrayBuffer.empty[(Int, Int, Int)]
  private var routeIndex = -1

  override def nextTurn(field: List[List[Int]]): (Int, Int, Int) = {
    if(route.isEmpty)
      solve(field, route)
    if (route.isEmpty)
      throw new RuntimeException("No solution found. Check if the raw sudoku is valid")
    routeIndex += 1
    route(routeIndex)
  }

  override def wrongTurn(message: String, field: List[List[Int]]): Unit = {
    throw new RuntimeException(s"Computer's turn is wrong, check out the algorithm. Field: ${SudokuUtils.renderSudoku(field)}")
  }

  def solve(field: List[List[Int]], route: ArrayBuffer[(Int, Int, Int)]): List[List[Int]] = {
    val emptyCell = findEmptyCell(field);
    if (emptyCell == null)
      return field

    val possibleValues = findPossibleValues(field, Option(emptyCell))

    for (value <- possibleValues) {
      val result = solve(field.updated(emptyCell._1, field(emptyCell._1).updated(emptyCell._2, value)), route)
      if (result != null) {
        route.addOne((emptyCell._1, emptyCell._2, value))
        return result
      }
    }
    null
  }

  private def findPossibleValues(field: List[List[Int]], cell: Option[(Int, Int)]): Array[Int] = {
    cell.map(cell => {
      val allPossibleValues = Set(1, 2, 3, 4, 5, 6, 7, 8, 9)
      val rowValues = Range(0, 9).map(field(cell._1)(_)).filter(_ > 0).toSet
      val colValues = Range(0, 9).map(field(_)(cell._2)).filter(_ > 0).toSet
      val squareValues = Range(0, 9).map(j => field((cell._1 / 3) * 3 + j / 3)((cell._2 / 3) * 3 + j % 3)).filter(_ > 0).toSet
      (allPossibleValues -- rowValues -- colValues -- squareValues).toArray
    }).getOrElse(Array.emptyIntArray)
  }

  private def findEmptyCell(field: List[List[Int]]): (Int, Int) = {
    Range(0, 9).flatMap(i => {
      Range(0, 9).map(j => (i, j))
    }).find(t => field(t._1)(t._2) == 0).orNull
  }
}
