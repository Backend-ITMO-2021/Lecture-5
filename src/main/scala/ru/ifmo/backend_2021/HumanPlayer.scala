package ru.ifmo.backend_2021
import scala.io.StdIn.readLine

/**
 * @author Vladimir Goncharov
 * @created 11.04.2021
 */
class HumanPlayer extends Player {
  override def nextTurn(field: List[List[Int]]): (Int, Int, Int) = {
    println(SudokuUtils.renderSudoku(field))
    println("Insert comma separated values (row, column, value)")
    val input = readLine()
    val splitInput = input.split(", ")
    (splitInput(0).toInt, splitInput(1).toInt, splitInput(2).toInt)
  }

  override def wrongTurn(message: String, field: List[List[Int]]): Unit = {
    println(message)
    println(SudokuUtils.renderSudoku(field))
  }
}
