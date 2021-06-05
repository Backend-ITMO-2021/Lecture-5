package ru.ifmo.backend_2021

import SudokuUtils.{isValidSudoku, renderSudoku, updateSudoku}

import scala.io.StdIn.readLine
import scala.util.Try

object Sudoku extends App {
  val sudokuInput =
    List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 2),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),

      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),

      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )

  def play(initialState: List[List[Int]] = sudokuInput): Unit = {
    while (initialState.flatten.exists(_ == 0)) { // пока в судоку есть 0, то есть пустые места
      println(renderSudoku(initialState)) // распечатаем судоку для игрока
      Try {
        println("введите строку [0,8]")
        val x = readLine().toInt
        println("введите столбец [1,9]")
        val y = readLine().toInt - 1
        println("введите значение значение [1,9]")
        val v = readLine().toInt
        require(x >= 0 && x <= 8, "значение должно быть от 0 до 8")
        require(y >= 0 && y <= 8, "значение должно быть от 1 до 9")
        require(v >= 1 && v <= 9, "значение должно быть от 1 до 9")

        require(initialState(x)(y) == 0, "ячейка для нового значения должна быть пустой")
        updateSudoku(initialState, x, y, v) match {
          case sudoku if isValidSudoku(sudoku) => play(sudoku)
          case sudoku =>
            println("Введенное значени некорректно")
            println(renderSudoku(sudoku))
            play(initialState)
        }
      } getOrElse {
        println("Введенные значения не являются корректными")
        play(initialState)
      }
    }
    println("Поздравляем!")
  }

    // play()
    Backtrack.backtrack(sudokuInput)
}