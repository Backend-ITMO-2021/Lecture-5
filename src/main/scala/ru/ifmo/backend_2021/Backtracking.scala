package ru.ifmo.backend_2021

class Backtracking(val grid: List[List[Int]]) {
  def solve(grid: List[List[Int]] = grid): List[List[Int]] = {
    for (x <- 0 to 8; y <- 0 to 8) {
      for (v <- 1 to 9) {
        if (grid(x)(y) == 0) {
          val solution = grid.updated(x, grid(x).updated(y, v))
          if (SudokuUtils.isValidSudoku(solution)) {
            if (SudokuUtils.isGameFinished(solution)) {
              print(SudokuUtils.renderSudoku(solution))
              println("Solution is found!")
              return solution
            }
            else return solve(solution)
          }
        }
      }
    }
    println("Failed! No solution found.")
    null
  }
}