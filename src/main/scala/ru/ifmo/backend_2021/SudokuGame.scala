package ru.ifmo.backend_2021

import java.util.InputMismatchException
import scala.io.StdIn.readLine

object SudokuGame {
  private def getInput(): Array[Int] = {
    println("Enter row number, column number and value divided by space")
    val input_regex = "^[0-8] [1-9] [1-9]$".r.unanchored
    val input = readLine()
    input match {
      case input_regex() => {
        val result = input.split{" "}.map{_.toInt}
        result.updated(1, result(1) - 1)
      }
      case _ => throw new InputMismatchException("Incorrect input, try again")
    }
  }

  private def checkStepIsOccupied(default_grid: List[List[Int]], row: Int, col: Int): Unit = {
    if (default_grid(row)(col) != 0)
      throw CustomExceptions.SudokuOccupiedException()
  }

  private def checkStepRules(grid: List[List[Int]], row: Int, col: Int, value: Int): Unit = {
    if (
      !SudokuUtils.isValidSudoku(
        applyStep(grid, row, col, value)
      )
    )
      throw CustomExceptions.SudokuRulesException()
  }

  private def applyStep(grid: List[List[Int]], row: Int, col: Int, value: Int): List[List[Int]] = {
    grid.updated(
      row,
      grid(row).updated(
        col,
        value
      )
    )
  }

  def run(default_grid: List[List[Int]]):Unit = {
    if (!SudokuUtils.isValidSudoku(default_grid)) throw new IllegalStateException("This sudoku grid is incorrect")

    println(
      SudokuUtils.renderSudoku(
        default_grid,
        added_numbers_coordinates = SudokuUtils.findZeros(default_grid)
      )
    )

    var grid = default_grid
    while (!SudokuUtils.isWin(grid)) {
      try {
        val Array(row: Int, col: Int, value: Int) = getInput()

        try {
          checkStepIsOccupied(default_grid, row, col)
          checkStepRules(grid, row, col, value)
          grid = applyStep(grid, row, col, value)
          println(
            SudokuUtils.renderSudoku(
              grid,
              added_numbers_coordinates = SudokuUtils.findZeros(default_grid)
            )
          )
        } catch {
          case e: CustomExceptions.SudokuOccupiedException => {
            println(
              SudokuUtils.renderSudoku(
                grid,
                added_numbers_coordinates = SudokuUtils.findZeros(default_grid),
                mistakes = Set((row, col))
              )
            )
            throw e
          }
          case e: CustomExceptions.SudokuRulesException => {
            println(
              SudokuUtils.renderSudoku(
                grid,
                added_numbers_coordinates = SudokuUtils.findZeros(default_grid),
                mistakes = SudokuUtils.findMistakes(applyStep(grid, row, col, value))
              )
            )
            throw e
          }
        }
      } catch {
        case e: Exception =>
          println(Console.RED + e.getMessage + Console.RESET)
      }
    }
    println(SudokuUtils.renderSudoku(grid, isWin = true))
  }
}
