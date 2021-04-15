package ru.ifmo.backend_2021

import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.util.control.Breaks.{break, breakable}

class Game(private val defGrid: List[List[Int]]) {

  private val grid: mutable.Seq[List[Int]] = ListBuffer.empty ++= defGrid

  def start(): Unit = {
    println(SudokuUtils.renderSudoku(grid.toList))
    while (!SudokuUtils.checkWin(grid.toList)) {
      try {
        breakable {
          println("Введите x")
          val x = scala.io.StdIn.readInt()
          println("Введите y")
          val y = scala.io.StdIn.readInt()
          println("Введите v")
          val v = scala.io.StdIn.readInt()

          if (!makeStep(x, y, v)) break

          println(SudokuUtils.renderSudoku(grid.toList))
        }
      } catch {
        case _: NumberFormatException =>
          println("Неверный формат ввода")
      }
    }
    println("GG")
  }

  private def makeStep(curX: Int, curY: Int, v: Int): Boolean ={
    val correctRange = 1 to 9

    if (!(correctRange contains curX) || !(correctRange contains curY) || !(correctRange contains v)) {
      println("Неверные значения в границах")
      return false
    }

    val x = curX - 1
    val y = curY - 1

    if (defGrid(x)(y) != 0){
      println("Ячейка уже занята изначально")
      return false
    }
    val prefValue = grid(x)(y)
    grid(x) = grid(x).updated(y, v)
    if (!SudokuUtils.isValidSudoku(grid.toList)){
      grid(x) = grid(x).updated(y, prefValue)
      println("Неверный ход по правилам")
      return false
    }

    true
  }

}
