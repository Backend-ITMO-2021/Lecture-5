package ru.ifmo.backend_2021

import ru.ifmo.backend_2021.SudokuUtils.{isValidSudoku, updateSudoku}

object Backtrack {
  def backtrack(initialSudokuState: List[List[Int]]): List[List[Int]] = {
    (0 to 8).map(x =>
      (0 to 8).map(y =>
        if (initialSudokuState(x)(y) == 0) {
          (1 to 9).map { v =>
            updateSudoku(initialSudokuState, x, y, v) match {
              case sudoku if isValidSudoku(sudoku) =>
                if (!sudoku.flatten.exists(_ == 0)) {
                  print(SudokuUtils.renderSudoku(sudoku))
                  return sudoku
                }
                else return backtrack(sudoku)
              case _ =>
            }
          }
        }
      )
    )

    println("Ошибка")
    List.empty
  }
}
