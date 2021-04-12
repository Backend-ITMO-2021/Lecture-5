package ru.ifmo.backend_2021

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

class SudokuGame(val field: List[List[Int]]) {
  def start(): Unit = {
    val sudoku = ListBuffer.empty ++= field
    println(SudokuUtils.renderSudoku(sudoku.toList))

    while (!isGameFinished(sudoku)) {
      println("Input row, col and value i.e. \"1 2 9\" (each index starts from 1!):")

      val input = readLine().split(" ")

      if (input.length != 3) {
        println("Incorrect input format!")
      } else {
        val x = input(0).toIntOption
        val y = input(1).toIntOption
        val v = input(2).toIntOption

        if (isInputWrong(x, y, v)) {
          println("Incorrect input value!")

        } else if (sudoku(x.get - 1)(y.get - 1) != 0) {
          println("Cell is not empty!")

        } else {
          sudoku(x.get - 1) = sudoku(x.get - 1).updated(y.get - 1, v.get)

          if (SudokuUtils.isValidSudoku(sudoku.toList)) {
            println(SudokuUtils.renderSudoku(sudoku.toList))
          } else {
            sudoku(x.get - 1) = sudoku(x.get - 1).updated(y.get - 1, 0)
            val duplicationCoordinates = SudokuUtils.getDuplicationCoordinates(sudoku.toList, x.get - 1, y.get - 1, v.get)
            println(SudokuUtils.renderSudoku(sudoku.toList, List(duplicationCoordinates)))
            println("Wrong value! Duplication is highlighted with '>' symbol.")
          }
        }
      }
    }

    println(SudokuUtils.renderSudoku(sudoku.toList))
    println("Congratulations!")
  }

  private def isInputWrong(x: Option[Int], y: Option[Int], v: Option[Int]): Boolean = {
    x.isEmpty || y.isEmpty || v.isEmpty ||
      x.get < 1 || x.get > 9 || y.get < 1 || y.get > 9 || v.get < 1 || v.get > 9
  }

  private def isGameFinished(sudoku: ListBuffer[List[Int]]): Boolean = {
    !sudoku.flatten.contains(0)
  }
}