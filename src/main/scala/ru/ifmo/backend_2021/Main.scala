package ru.ifmo.backend_2021

object Main extends App {
  val longPlayPosition: List[List[Int]] = List(
    List(0, 0, 7, 5, 0, 2, 9, 0, 0),
    List(0, 9, 6, 0, 8, 0, 0, 1, 0),
    List(8, 0, 4, 0, 0, 0, 6, 2, 7),
    List(7, 0, 0, 8, 0, 5, 0, 0, 3),
    List(0, 4, 0, 0, 1, 0, 0, 6, 0),
    List(9, 0, 0, 6, 0, 7, 0, 0, 1),
    List(1, 7, 9, 0, 0, 0, 2, 0, 6),
    List(0, 3, 0, 0, 7, 0, 1, 8, 0),
    List(0, 0, 2, 3, 0, 1, 5, 0, 0)
  )

  val fastPlayPosition: List[List[Int]] = List(
    List(3, 1, 7, 5, 6, 2, 9, 4, 8),
    List(2, 9, 6, 7, 8, 4, 3, 1, 5),
    List(8, 5, 4, 1, 3, 9, 6, 2, 7),
    List(7, 6, 1, 8, 2, 5, 4, 9, 3),
    List(5, 4, 8, 9, 1, 3, 7, 6, 2),
    List(9, 2, 3, 6, 4, 7, 8, 5, 1),
    List(1, 7, 9, 4, 5, 8, 2, 3, 6),
    List(4, 3, 5, 2, 7, 6, 1, 8, 9),
    List(6, 8, 2, 3, 9, 1, 5, 0, 0)
  )

  Game.gameLoop(HumanPlayer, longPlayPosition, longPlayPosition)
}