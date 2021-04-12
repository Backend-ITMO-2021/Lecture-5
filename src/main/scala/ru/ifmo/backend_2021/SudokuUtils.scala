package ru.ifmo.backend_2021

import scala.io.StdIn.readLine

object SudokuUtils {
  def main(args: Array[String]): Unit = {
    val grid =
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
    println(renderSudoku(grid))
    val solvedSudoku = solveSudoku(grid)
    _game(grid, solvedSudoku)
  }

  def hasEqual(line: List[Int]): Boolean = {
    line.filter(x => x != 0).groupBy(x => x).map(_._2.size).exists(x => x > 1)
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    rawSudoku.foreach( row =>
      if (hasEqual(row)) {
        return false
      }
    )
    rawSudoku.transpose.foreach( col =>
      if (hasEqual(col)) {
        return false
      }
    )
    val boxes = rawSudoku.zipWithIndex.map{ case(x, i) =>
      x.zipWithIndex.map{ case(y, j) =>
        rawSudoku((i/3)*3 + j/3)((i%3)*3+j%3)
      }
    }
    boxes.foreach( box =>
      if (hasEqual(box)) {
        return false
      })
    true
  }

  def sudokuStringBuilder(grid: List[List[Int]], string: String, row: Int, col: Int, changeRow: Boolean): String = {
    if (row % 3 == 0 & changeRow & row <= 8) {
      val newString = string.concat("\n--+-------+-------+-------+\n" + row + " ")
      sudokuStringBuilder(grid, newString, row, col, changeRow = false)
    } else if (row % 3 != 0 & changeRow & row <= 8) {
      val newString = string.concat("\n" + row + " ")
      sudokuStringBuilder(grid, newString, row, col, changeRow = false)
    } else if (!changeRow & row <= 8) {
      if (col % 3 == 0 & col < 8) {
        if (grid(row)(col) == 0) {
          val newString = string.concat("| " + "  ")
          sudokuStringBuilder(grid, newString, row, col+1, changeRow = false)
        } else if (grid(row)(col) < 0) {
          val newString = string.concat("| " + "[" + -grid(row)(col) + "]")
          sudokuStringBuilder(grid, newString, row, col+1, changeRow = false)
        } else {
          val newString = string.concat("| " + grid(row)(col) + " ")
          sudokuStringBuilder(grid, newString, row, col+1, changeRow = false)
        }
      } else if (col == 8) {
        if (grid(row)(col) == 0) {
          val newString = string.concat("  |")
          sudokuStringBuilder(grid, newString, row + 1, 0, changeRow = true)
        } else if (grid(row)(col) < 0) {
          val newString = string.concat("[" + -grid(row)(col) + "]|")
          sudokuStringBuilder(grid, newString, row + 1, 0, changeRow = true)
        } else {
          val newString = string.concat(grid(row)(col) + " |")
          sudokuStringBuilder(grid, newString, row + 1, 0, changeRow = true)
        }
      } else if (col % 3 != 0 & col < 8) {
        if (grid(row)(col) == 0) {
          val newString = string.concat("  ")
          sudokuStringBuilder(grid, newString, row, col+1, changeRow = false)
        } else if (grid(row)(col) < 0) {
          val newString = string.concat("[" + -grid(row)(col) + "]")
          sudokuStringBuilder(grid, newString, row, col+1, changeRow = false)
        } else {
          val newString = string.concat(grid(row)(col) + " ")
          sudokuStringBuilder(grid, newString, row, col+1, changeRow = false)
        }
      } else {
        string
      }
    } else {
      string
    }
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    val start = sudokuStringBuilder(grid, "\n  | 1 2 3 | 4 5 6 | 7 8 9 |", 0, 0, changeRow = true)
    val field = start.concat("\n--+-------+-------+-------+\n")
    println(field)
    field
  }

  def findEmpty(grid: List[List[Int]]): List[Int] = {
    grid.zipWithIndex.foreach{ case (x, row) =>
      val col = x.indexOf(0)
      if (col != -1) {
        return List(row, col)
      }
    }
    List()
  }

  def changeValue(grid: List[List[Int]], row: Int, col: Int, value: Int): List[List[Int]] = {
    grid.updated(row, grid(row).updated(col, value))
  }

  def solveSudoku(grid: List[List[Int]]): List[List[Int]] = {
    val emptyPos = findEmpty(grid)
    if (emptyPos.isEmpty) {
      return grid
    }
    val row = emptyPos.head
    val col = emptyPos(1)
    for (i <- 1 to 9) {
      val changedGrid = changeValue(grid, row, col, i)
      if (isValidSudoku(changedGrid)) {
        val resultGrid = solveSudoku(changedGrid)
        if (resultGrid.nonEmpty) {
          return resultGrid
        }
      }
    }
    List()
  }

  def isValidInput(string: String): Boolean = {
    val len = string.replaceAll(" ", "").length
    if (len == 3 & string.replaceAll(" ", "").matches("[1-9]+")) {
        return true
      }
    println("Invalid input. Try again:")
    false
  }

  def inputBuilder(string: String): String = {
    if (!isValidInput(string)) {
      val newString = readLine()
      return inputBuilder(newString)
    }
    string
  }

  def _game(grid: List[List[Int]], solvedSudoku: List[List[Int]]): Any = {
    println("Enter the number of the row, column, and the number you want to put in the cell, separated by a space:")

    val testInput = readLine()
    val input = inputBuilder(testInput).replaceAll(" ", "").split("")

    val changedSudoku = changeValue(grid, input(0).toInt-1, input(1).toInt-1, input(2).toInt)
    if (changedSudoku == solvedSudoku) {
      println("\nYou have solved sudoku!")
      renderSudoku(changedSudoku)
      changedSudoku
    } else if (isValidSudoku(changedSudoku)) {
      println("\nIntermediate result:")
      renderSudoku(changedSudoku)
      _game(changedSudoku, solvedSudoku)
    } else if (!isValidSudoku(changedSudoku)) {
      println("\nYou made a mistake:")
      renderSudoku(changeValue(grid, input(0).toInt-1, input(1).toInt-1, -input(2).toInt))
      _game(changedSudoku, solvedSudoku)
    }
  }

}