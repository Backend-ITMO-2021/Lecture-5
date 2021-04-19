package ru.ifmo.backend_2021

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

  def isFinish(field: List[List[Int]]): Boolean = {
    field.foreach(row => {
      if (row.contains(0)) {
        return false
      }
    })
    true
  }

  def play(): Unit = {
    println("Sudoku game!")
    println("Enter x y v:")
    println(SudokuUtils.renderSudoku(box))
    step(box)
    println("Done!")
  }

  def step(oldField: List[List[Int]]): Option[List[List[Int]]] = {
    readUserInput() match {
      case Some(values) => {
        val (x, y ,v) = values
        if (!isInputCorrect(x, y ,v)) {
          println("Incorrect values!");
          step(oldField)
        } else {
          val oldRow = oldField(y)
          val newRow = oldRow.updated(x - 1, v)
          val newField = oldField.updated(y, newRow)
          if (SudokuUtils.isValidSudoku(newField)) {
            println(SudokuUtils.renderSudoku(newField))
            if (!isFinish(newField)) {
              step(newField)
            } else {
              None
            }
          } else {
            println("Wrong cell value!")
            val restoredField = newField.updated(y, oldRow)
            step(restoredField)
          }
        }
      }
      case None => {
        println("Incorrect input!")
        step(oldField)
      }
    }
  }
}
