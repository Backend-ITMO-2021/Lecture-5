package ru.ifmo.backend_2021

import scala.io.StdIn.readLine

class Game(val grid: List[List[Int]]) {
  def start(grid: List[List[Int]] = grid): List[List[Int]] = {
    println(SudokuUtils.renderSudoku(grid))

    if (!SudokuUtils.isCompleted(grid)) {
      println("Введите строку, столбец и значение через пробел")
      val input = readLine().split(" ")
      if (input.length != 3) {
        println("Введите 3 числа")
        start(grid)
      } else {
        try {
          val row = input(0).toInt
          val col = input(1).toInt - 1
          val value = input(2).toInt
          if (isInputIncorrect(row, col, value)) {
            println("Числа выходят за допустимые пределы")
            start(grid)
          }
          else if (grid(row)(col) != 0) {
            println("Это ячейка уже заполнена")
            start(grid)
          }
          else {
            val updatedGrid = grid.updated(row, grid(row).updated(col, value))
            if (SudokuUtils.isValidSudoku(updatedGrid)) {
              start(updatedGrid)
            }
            else {
              val errors = checkRow(updatedGrid, row, value) ++ checkCol(grid, col, value) ++ checkSquare(grid, row, col, value)

              println("Значение неверное:")
              println(SudokuUtils.renderSudoku(updatedGrid, value, errors))

              start(grid)
            }
          }
        } catch {
          case _: Throwable =>
            println("Некорректный ввод (вводите числа!)")
            start(grid)
        }
      }
    } else {
      println("Решено!")
      null
    }
  }

  def isInputIncorrect(row: Int, col: Int, value: Int): Boolean =
    row > 8 || row < 0 || col > 8 || col < 0 || value > 9 || value < 1

  def checkRow(grid: List[List[Int]], row: Int, value: Int): IndexedSeq[(Int, Int)] =
    for (i <- 0 to 8 if grid(row)(i) == value) yield (row, i)

  def checkCol(grid: List[List[Int]], col: Int, value: Int): IndexedSeq[(Int, Int)] =
    for (i <- 0 to 8 if grid(i)(col) == value) yield (i, col)

  def checkSquare(grid: List[List[Int]], row: Int, col: Int, value: Int): IndexedSeq[(Int, Int)] = {
    val squareRowStartIndex = (row / 3) * 3
    val squareColStartIndex = (col / 3) * 3
    for (i <- 0 to 8 if grid(squareRowStartIndex + (i / 3))(squareColStartIndex + (i % 3)) == value)
      yield (squareRowStartIndex + (i / 3), squareColStartIndex + (i % 3))
  }
}
