package ru.ifmo.backend_2021

import ru.ifmo.backend_2021.game._

/**
 * @author https://github.com/trangology
 */


object Sudoku {
    def main(args: Array[String]): Unit = {
        Thread.sleep(3000)
        println("\nHey Ilya, you are about to play your own Sudoku puzzle!\n")
        Thread.sleep(3000)
        println("I honestly do not get the idea of indexing from 0 for the rows but from 1 for the columns.\n" +
            "This makes my app logic more challenging to handle, you know?\n")
        Thread.sleep(5000)
        println("However, this game is special designed for just you - not for me. So, have fun!\n")
        Thread.sleep(3000)
        println("Btw, any review/bug report of this program are welcome.\n")
        Thread.sleep(3000)

        val puzzle = new Puzzle()
        Game(Human, puzzle, GET_STARTED).play()
        Game(Solver, puzzle, GET_STARTED).play()
    }
}
