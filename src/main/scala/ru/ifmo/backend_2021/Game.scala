package ru.ifmo.backend_2021
import scala.util.control.Breaks.break

class Game(var field: List[List[Int]], player: Player = new Human) {

  def play(): Unit = {
    println(SudokuUtils.renderSudoku(field))
    while (!isCompleted(field)) {
      val step = player.makeStep(field)
      if (step == null) {
        noSolution()
        break
      }
      val row = step(0)
      val col = step(1)
      val value = step(2)
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
    }
    println(Console.GREEN + "Задача решена" + Console.RESET)
  }

  private def isCompleted(field: List[List[Int]]): Boolean = {
    !field.flatten.contains(0)
  }

  private def noSolution(): Unit = {
    println(Console.RED + "Нет решения" + Console.RESET)
  }

//  def correctInput(input: Array[String]): Boolean = {
//    input.length == 3 && input(0).toInt > 0 && input(0).toInt < 10 && input(1).toInt >
//      0 && input(1).toInt < 10 && input(2).toInt > 0 && input(2).toInt < 10
//  }

}
