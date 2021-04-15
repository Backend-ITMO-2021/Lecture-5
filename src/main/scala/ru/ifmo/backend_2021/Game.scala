package ru.ifmo.backend_2021
import scala.util.control.Breaks.break

class Game(val field: List[List[Int]], player: Player = new Human) {

  def sudokuStep(field: List[List[Int]]): Unit = {
    val step = player.makeStep(field)
    if (step == null) {
      if (!isCompleted(field)) {
        noSolution()
        break
      }
    } else {
      val row = step(0)
      val col = step(1)
      val value = step(2)
      if (field(row)(col - 1) != 0) {
        println(Console.RED + "Клетка занята" + Console.RESET)
        println(SudokuUtils.renderSudoku(field, List((row, col - 1))))
        sudokuStep(field)
      } else {
        val curField = field.updated(row, field(row).updated(col - 1, value))
        if (!SudokuUtils.isValidSudoku(curField)) {
          println(Console.RED + "Некорректное значение" + Console.RESET)
          val incorrectPos = SudokuUtils.incorrectStep(curField, row, col - 1, value)
          println(SudokuUtils.renderSudoku(curField, incorrectPos))
          println(SudokuUtils.renderSudoku(field))
          sudokuStep(field)
        }
        println(SudokuUtils.renderSudoku(curField))
        sudokuStep(curField)
      }
    }
  }

  def play(): Unit = {
    println(SudokuUtils.renderSudoku(field))
    sudokuStep(field)
    println(Console.GREEN + "Задача решена" + Console.RESET)
  }


  private def isCompleted(field: List[List[Int]]): Boolean = {
    !field.flatten.contains(0)
  }

  private def noSolution(): Unit = {
    println(Console.RED + "Нет решения" + Console.RESET)
  }

}
