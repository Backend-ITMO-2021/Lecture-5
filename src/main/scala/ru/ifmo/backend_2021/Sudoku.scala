package ru.ifmo.backend_2021

import scala.collection.mutable.ListBuffer
import scala.io.StdIn.readLine

class Sudoku(val box: List[List[Int]]) {

  def readUserInput(): Option[(Int, Int, Int)] = {
    val input = readLine()
    val inputElements = input.split(" ")
    if (inputElements.length != 3) {
      return None
    }
    val x = inputElements(0).toInt
    val y = inputElements(1).toInt
    val v = inputElements(2).toInt
    Some((x, y, v))
  }

  def isInputCorrect(x: Int, y: Int, v: Int): Boolean = (x >= 1) && (x <= 9) && (y >= 1) && (y <= 9) && (v >= 1) && (v <= 9)

  def isFinish(): Boolean = {
    box.foreach(row => {
      if (row.contains(0)) {
        return false
      }
    })
    true
  }

  def play(): Unit = {
    val field = ListBuffer.from(box)
    println("Sudoku game!")
    println("Enter x y v:")
    println(SudokuUtils.renderSudoku(field.toList))
    while (!isFinish()) {
      readUserInput() match {
        case Some(values) => {
          val (x, y ,v) = values
          if (!isInputCorrect(x, y ,v)) {
            println("Incorrect values!");
          } else {
            field(y) = field(y).updated(x - 1, v)
            if (SudokuUtils.isValidSudoku(field.toList)) {
              println(SudokuUtils.renderSudoku(field.toList))
            } else {
              field(y) = field(y).updated(x - 1, 0)
              println("Wrong cell value!")
            }
          }
        }
        case None => println("Incorrect input!")
      }
    }
    println("Done!")
  }
}
