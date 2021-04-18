package ru.ifmo.backend_2021

import scala.collection.mutable
import scala.io.StdIn.readLine
import scala.collection.mutable.ListBuffer

class Game(val grid: List[List[Int]]) {

  def start(): Unit = {
    gameState(grid)
  }

  def gameState(grid: List[List[Int]]): Unit = {
    println(SudokuUtils.renderSudoku(grid))

    while (!SudokuUtils.isGameFinished(grid)) {
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

        val updatedGrid = grid.patch(x, Seq(grid(x).patch(y, Seq(v), 1)), 1)

        if (!SudokuUtils.isValidSudoku(updatedGrid)) {
          throw new Exception("Value " + v + " is invalid in given position.")
        }

        gameState(updatedGrid)
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