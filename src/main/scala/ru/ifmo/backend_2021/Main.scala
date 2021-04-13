package ru.ifmo.backend_2021

import scala.annotation.tailrec

object Main {

  def main(args: Array[String]): Unit = {
    println("Sudoku:")

    if (steps(List.fill(9)(List.fill(9)(0)))) {
      println("Congratulations!")
    }

    println("Bye!")
  }

  @tailrec
  def steps(field: List[List[Int]]): Boolean = {
    print(SudokuUtils.renderSudoku(field))

    val finalField = try {
      val line = try {
        Console.in.readLine().trim.split("\\s+").map(Integer.parseInt)
      } catch {
        case e: NumberFormatException => throw new IllegalArgumentException("Not an integers")
      }

      if (line.length != 3) {
        throw new IllegalArgumentException("Amount of numbers is not 3")
      }

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

      if (SudokuUtils.isFilledSudoku(newField)) {
        print(SudokuUtils.renderSudoku(field))
        return true
      }

      newField
    } catch {
      case e: IllegalArgumentException =>
        println(e.getMessage)
        field

      case e: NullPointerException => return false
    }

    steps(finalField)
  }
}
