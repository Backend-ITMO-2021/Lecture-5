package ru.ifmo.backend_2021

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

class SudokuGame(val field: List[List[Int]]) {

  def start(): Unit = {
    val sudoku = ListBuffer.empty ++= field
    println(SudokuUtils.renderSudoku(sudoku.toList))
    while (!isCompleted()) {
      println("Введите строку, столбец и значение через пробел")
      val input = readLine().split(" ")
      if (input.length != 3) {
        println("Некорректный ввод")
      } else {
        val x = input(0).toIntOption
        val y = input(1).toIntOption
        val v = input(2).toIntOption
        if (isNotInputCorrect(x, y, v) || sudoku(x.get - 1)(y.get - 1) != 0)
        {
          println("Некорректный ввод")
        }
        else
        {
          sudoku(x.get - 1) = sudoku(x.get - 1).updated(y.get - 1, v.get)
          if (SudokuUtils.isValidSudoku(sudoku.toList)) {
            println(SudokuUtils.renderSudoku(sudoku.toList))
          } else {
            sudoku(x.get - 1) = sudoku(x.get - 1).updated(y.get - 1, 0)
            println("Неправильное значение")
          }
        }
      }
    }
    println("Решено")
  }

  private def isNotInputCorrect(x: Option[Int], y: Option[Int], v: Option[Int]): Boolean =
    x.isEmpty || y.isEmpty || v.isEmpty || x.get < 1 || x.get > 9 || y.get < 1 || y.get > 9 || v.get < 1 || v.get > 9

  def isCompleted(): Boolean={
    field.foreach(row => {
      if(row.contains(0)){
        return false
      }
    })
    true
  }
}
