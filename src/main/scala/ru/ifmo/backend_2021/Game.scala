package ru.ifmo.backend_2021
import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer

class Game(val field: List[List[Int]]) {

  def isFinished(): Boolean = {
    for (row <- field){
      if (row.contains(0)) {
        return false
      }
    }
    true
  }

  def run(): Unit = {
    val sudoku = ListBuffer.empty ++= field
    println(SudokuUtils.renderSudoku(sudoku.toList))
    while (!isFinished()) {
      println("Введите строку, столбец и значение через пробел")
      val input = readLine().split(" ")
      val x = input(0).toInt
      val y = input(1).toInt
      val value = input(2).toInt
      if (sudoku(x)(y - 1) == 0) {
        sudoku(x) = sudoku(x).updated(y - 1, value)
        if (SudokuUtils.isValidSudoku(sudoku.toList)) {
          println(SudokuUtils.renderSudoku(sudoku.toList))
        } else {
          sudoku(x) = sudoku(x).updated(y - 1, 0)
          println("Неправильное значение")
        }
      }
      else {
        println("Проверьте вводимые значения")
      }
    }
    println("Победа!")
  }
}
