package ru.ifmo.backend_2021

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

class Game(val grid: List[List[Int]]) {
  def start() = {
    val sudoku = ListBuffer.empty ++= grid
    println(SudokuUtils.renderSudoku(sudoku.toList))

    while (!isCompleted(sudoku.toList)) {
      println("Введите строку, столбец и значение через пробел")
      val input = readLine().split(" ")
      if (input.length != 3) {
        println("Введите 3 числа")
      } else {
        try {
          val row = input(0).toInt
          val col = input(1).toInt
          val value = input(2).toInt
          if (isInputIncorrect(row, col, value)) println("Числа выходят за допустимые пределы")
          else if (sudoku(row)(col - 1) != 0) println("Это ячейка уже заполнена")
          else {
            sudoku(row) = sudoku(row).updated(col - 1, value)
            if (SudokuUtils.isValidSudoku(sudoku.toList)) println(SudokuUtils.renderSudoku(sudoku.toList))
            else {
              sudoku(row) = sudoku(row).updated(col - 1, 0)
              println("Значение неверное")
            }
          }
        } catch {
          case _: Throwable => println("Некорректный ввод (вводите числа!)")
        }
      }
    }
    println("Решено")
  }

  def isCompleted(grid: List[List[Int]]): Boolean = {
    grid.foreach(row => {
      if (row.contains(0)) return false
    })
    true
  }

  def isInputIncorrect(row: Int, col: Int, value: Int): Boolean =
    row > 8 || row < 0 || col > 9 || col < 1 || value > 9 || value < 1
}
