package ru.ifmo.backend_2021

import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer

class Game(val field: List[List[Int]]) {
  private val grid: mutable.Seq[List[Int]] = ListBuffer.empty ++= field

  def start(): Unit = {
    println(SudokuUtils.renderSudoku(grid.toList))

    while (!SudokuUtils.isGameFinished(grid.toList)) {
      try {
        val input = readLine().split(" ").map(Integer.parseInt)

        if (input.length != 3) {
          throw new Exception("Amount of input is not enough.")
        }

        val x = input(0)
        val y = input(1) - 1
        val v = input(2)


        if (SudokuUtils.isInputWrong(x, y, v)) {
          throw new Exception("Input arguments are not valid.")
        }

        if (grid(x)(y) != SudokuUtils.EMPTY_VALUE) {
          throw new Exception("Position is already filled with value.")
        }

        grid(x) = grid(x).updated(y, v)

        if (!SudokuUtils.isValidSudoku(grid.toList)) {
          grid(x) = grid(x).updated(y, 0)
          throw new Exception("Value " + v + " is invalid in given position.")
        }

        println(SudokuUtils.renderSudoku(grid.toList))
      }
      catch {
        case e: Exception => {
          println(e.getMessage)
        }
      }
    }

    println("Good game!")

  }
}