package ru.ifmo.backend_2021

import ru.ifmo.backend_2021.SudokuGame
import ru.ifmo.backend_2021.BacktrackingPlayer

object Main {
  def main(args: Array[String]): Unit = {
    val startField =
      List(
        List(6, 5, 7,   9, 4, 1,   2, 3, 8),
        List(1, 2, 3,   6, 5, 8,   9, 4, 7),
        List(8, 9, 4,   2, 3, 7,   6, 5, 1),

        List(0, 6, 5,   1, 2, 3,   4, 8, 9),
        List(2, 3, 1,   8, 9, 4,   5, 7, 6),
        List(9, 4, 8,   7, 6, 5,   0, 2, 3),

        List(5, 1, 2,   3, 7, 6,   8, 9, 4),
        List(3, 8, 9,   4, 1, 2,   7, 6, 5),
        List(4, 7, 6,   5, 8, 9,   3, 0, 2)
      )

    // Manual
//    val game = new SudokuGame(startField)
//    game.start()

    // Backtracking Player
    val player = new BacktrackingPlayer(startField)
    player.solve()
  }
}
