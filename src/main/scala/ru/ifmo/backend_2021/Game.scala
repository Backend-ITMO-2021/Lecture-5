package ru.ifmo.backend_2021
import scala.io.StdIn.readLine

class Game(grid: List[List[Int]]) {

  def play(grid: List[List[Int]] = grid): Unit = {
      println(SudokuUtils.renderSudoku(grid))
      while (shouldContinue()) {
        try {
          println("Insert column, row and value according to format x y v:")
          val input = readLine().split(" ").map(Integer.parseInt)
          if (input.length != 3) throw new Exception("Incorrect input")
          
        val x = input(0) - 1
        val y = input(1)
        val v = input(2)
        
        if (x > 8 || y > 8 || v > 9) throw new Exception("Chosen cell or value are out of bounds")

        if (grid(y)(x) != 0) {
          throw new Exception("Chosen cell already has a value")
        }

        val newField = grid.patch(y, Seq(grid(y).patch(x, Seq(v), 1)), 1) 
        if (!SudokuUtils.isValidSudoku(newField)) {
          throw new Exception("Value " + v + " is invalid in position: col-" + (x+1).toString() + ", row-" + y.toString())
        }
        play(newField)
      } 
      catch {
        case e: Exception => {
          println(e.getMessage)
        }
      } 
  }
}
  def shouldContinue(): Boolean = {
    grid.foreach(row => {
      if(row.contains(0)){
        return true
      }
    })
    false
  }
}


