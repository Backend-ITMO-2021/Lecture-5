package ru.ifmo.backend_2021

import java.io.EOFException

object Main {

  def main(args: Array[String]): Unit = {
    println("Sudoku:")

    var field = List.fill(9)(List.fill(9)(0))
    try {
      while (true) {
        print(SudokuUtils.renderSudoku(field))

        try {
          val line = Console.in.readLine().split(" ").map(Integer.parseInt)
          val x = line(0)
          val y = line(1)
          val v = line(2)

          if (x < 1 || x > 9) {
            throw new IllegalArgumentException("X could be only from 1 to 9")
          }

          if (y < 0 || y > 8) {
            throw new IllegalArgumentException("Y could be only from 0 to 8")
          }

          if (v < 1 || v > 9) {
            throw new IllegalArgumentException("V could be only from 1 to 9")
          }

          val newField = field.updated(y, field(y).updated(x - 1, v))
          if (!SudokuUtils.isValidSudoku(newField)) {
            throw new IllegalArgumentException(v + " cannot be placed on this place")
          }

          field = newField
        } catch {
          case e: NumberFormatException => println("Not an integer")
          case e: IllegalArgumentException => println(e.getMessage)
        }
      }
    } catch {
      case e: NullPointerException => println("Bye!")
    }
  }
}
