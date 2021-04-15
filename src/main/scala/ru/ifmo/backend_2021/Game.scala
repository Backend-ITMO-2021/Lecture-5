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
          val col = input(1).toInt - 1
          val value = input(2).toInt
          if (isInputIncorrect(row, col, value)) println("Числа выходят за допустимые пределы")
          else if (sudoku(row)(col) != 0) println("Это ячейка уже заполнена")
          else {
            sudoku(row) = sudoku(row).updated(col, value)
            if (SudokuUtils.isValidSudoku(sudoku.toList)) println(SudokuUtils.renderSudoku(sudoku.toList))
            else {
              val incorrectSudoku = ListBuffer.empty ++= sudoku
              val squareRowStartIndex = (row / 3) * 3
              val squareColStartIndex = (col / 3) * 3
              incorrectSudoku(row) = incorrectSudoku(row).updated(col, -1)
              Range(0, 9).foreach(i => {
                val currentSquareRow = squareRowStartIndex + (i / 3)
                val currentSquareCol = squareColStartIndex + (i % 3)
                if (incorrectSudoku(row)(i) == value) incorrectSudoku(row) = incorrectSudoku(row).updated(i, -1)
                if (incorrectSudoku(i)(col) == value) incorrectSudoku(i) = incorrectSudoku(i).updated(col, -1)
                if (incorrectSudoku(currentSquareRow)(currentSquareCol) == value)
                  incorrectSudoku(currentSquareRow) = incorrectSudoku(currentSquareRow).updated(currentSquareCol, -1)
              })

              sudoku(row) = sudoku(row).updated(col, 0)
              println("Значение неверное:")
              println(SudokuUtils.renderSudoku(incorrectSudoku.toList, value))
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
    row > 8 || row < 0 || col > 8 || col < 0 || value > 9 || value < 1
}
