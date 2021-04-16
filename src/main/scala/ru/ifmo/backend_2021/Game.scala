package ru.ifmo.backend_2021

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

class Game(private val defGrid: List[List[Int]]) {

  def start(): Unit = {
    println(SudokuUtils.renderSudoku(defGrid))
    play(defGrid)
  }

  private def play(grid: List[List[Int]]) {
    if (!SudokuUtils.checkWin(grid)) {
      try {
        println("Введите x")
        val x = scala.io.StdIn.readInt()
        println("Введите y")
        val y = scala.io.StdIn.readInt()
        println("Введите v")
        val v = scala.io.StdIn.readInt()
        val newGrid = makeStep(x, y, v, grid)
        println(SudokuUtils.renderSudoku(newGrid))
        play(newGrid)
      } catch {
        case _: NumberFormatException =>
          println("Неверный формат ввода")
          play(grid)
      }
    }
    println("GG")
  }

  private def makeStep(curX: Int, curY: Int, v: Int, grid: List[List[Int]]): List[List[Int]] = {
    val correctRange = 1 to 9

    if (!(correctRange contains curX) || !(correctRange contains curY) || !(correctRange contains v)) {
      println("Неверные значения в границах")
      return grid
    }

    val x = curX - 1
    val y = curY - 1

    if (defGrid(x)(y) != 0) {
      println("Ячейка уже занята изначально")
      return grid
    }
    val prefValue = grid(x)(y)
    val newGrid = createNewGrid(grid, x, y, v)
    if (!SudokuUtils.isValidSudoku(newGrid)) {
      val newGrid = createNewGrid(grid, x, y, prefValue)
      println("Неверный ход по правилам")
      return newGrid
    }
    println(newGrid)
    newGrid
  }

  def createNewGrid(grid: List[List[Int]], x: Int, y: Int, v: Int): List[List[Int]] =
    grid.patch(x, Seq(grid(x).patch(y, Seq(v), 1)), 1)

}
