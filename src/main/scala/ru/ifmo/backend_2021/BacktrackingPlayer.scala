package ru.ifmo.backend_2021

import scala.collection.mutable.ListBuffer

class BacktrackingPlayer(val field: List[List[Int]]) {
  def solve(x: Int = -1, y: Int = -1, currentField: List[List[Int]] = field): List[(Int, Int, Int)] = {

    for (i <- currentField.indices) {
      for (j <- currentField(i).indices) {
        if (currentField(i)(j) == 0) {

          // Found next empty cell to brute force values

          for (v <- 1 to 9) {
            if (SudokuUtils.isValidSudoku(updatedField(currentField, i, j, v))) {
              val furtherSolution = solve(i, j, updatedField(currentField, i, j, v))

              if (furtherSolution != null) {
                return List((i, j, v)) ++ furtherSolution
              } else {
                if (!updatedField(currentField, i, j, v).flatten.contains(0)) {

                  println(SudokuUtils.renderSudoku(updatedField(currentField, i, j, v)))
                  println("Finished!")
                  return List((i, j, v))
                }
              }
            }
          }

          // Reached 9 but field still wrong

          if (x == -1 && y == -1) {
            println("Unable to solve!")
          }

          return null
        }
      }
    }

    // No empty cells anymore, end of game
    null
  }

  private def updatedField(field: List[List[Int]], i: Int, j: Int, v: Int): List[List[Int]] = {
    field.updated(i, field(i).updated(j, v))
  }
}