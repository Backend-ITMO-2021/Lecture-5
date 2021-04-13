package ru.ifmo.backend_2021

import scala.util.Try

trait Player {
  def move: Try[(Int, Int, Int)]
}

object HumanPlayer extends Player {
  override def move: Try[(Int, Int, Int)] = Try {
    val data = scala.io.StdIn.readLine().split(" ").map(_.toInt)
    (data(0), data(1) - 1, data(2))
  }
}