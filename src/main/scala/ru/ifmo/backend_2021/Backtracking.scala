package ru.ifmo.backend_2021

class Backtracking(val grid: List[List[Int]]) {
  def solve(grid: List[List[Int]] = grid): List[List[Int]] = {
    for (row <- 0 to 8; col <- 0 to 8 if grid(row)(col) == 0) {
      for (value <- 1 to 9) {
        val updatedGrid = grid.updated(row, grid(row).updated(col, value))
        if (SudokuUtils.isValidSudoku(updatedGrid)) {
          if (SudokuUtils.isCompleted(updatedGrid)) {
            println("Решение найдено:")
            print(SudokuUtils.renderSudoku(updatedGrid))
            return updatedGrid
          }
          else return solve(updatedGrid)
        }
      }
    }
    println("Не удалось найти решение :с\nПроверьте правильность судоку!")
    null
  }
}
