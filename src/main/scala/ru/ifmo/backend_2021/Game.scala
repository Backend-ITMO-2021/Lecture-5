package ru.ifmo.backend_2021
import scala.io.StdIn.readLine

class Game(var field: List[List[Int]]) {

  def play(): Unit = {
    println(SudokuUtils.renderSudoku(field))
    while (!isCompleted(field)) {
      println("Введите через пробел строку, столбец и значение")
      val input = readLine().split(" ")
      if (correctInput(input)) {
        val row = input(0).toInt
        val col = input(1).toInt
        val value = input(2).toInt
        if (field(row)(col - 1) != 0) {
          println(Console.RED + "Клетка занята" + Console.RESET)
          println(SudokuUtils.renderSudoku(field, List((row, col - 1))))
        } else {
          field = field.updated(row, field(row).updated(col - 1, value))
          if (!SudokuUtils.isValidSudoku(field)) {
            println(Console.RED + "Некорректное значение" + Console.RESET)
            val incorrectPos = SudokuUtils.incorrectStep(field, row, col - 1, value)
            println(SudokuUtils.renderSudoku(field, incorrectPos))
            field = field.updated(row, field(row).updated(col - 1, 0))
          }
          println(SudokuUtils.renderSudoku(field))
        }

      } else {
        println(Console.RED + "Некорректный формат ввода" + Console.RESET)
      }

    }

  }

  def isCompleted(field: List[List[Int]]): Boolean = {
    !field.flatten.contains(0)
  }

  def correctInput(input: Array[String]): Boolean = {
    input.length == 3 && input(0).toInt > 0 && input(0).toInt < 10 && input(1).toInt >
      0 && input(1).toInt < 10 && input(2).toInt > 0 && input(2).toInt < 10
  }

}
