package ru.ifmo.backend_2021

import ru.ifmo.backend_2021.SudokuUtils.{isValidSudoku, renderSudoku, validInput, validMove}
import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

sealed trait GameStatus

case object InProcess extends GameStatus

case object End extends GameStatus

object Game {

  def checkGameStatus(sudoku: List[List[Int]]): GameStatus = {
    if (sudoku.exists(_.exists(_ == 0)) || !isValidSudoku(sudoku)) InProcess else End
  }

  def updateBoard(move: (Int, Int, Int), sudoku: List[List[Int]]): List[List[Int]] = {
    sudoku.updated(move._1, sudoku(move._1).updated(move._2, move._3))
  }

  def nextState(move: Try[(Int, Int, Int)], currentPosition: List[List[Int]], startPosition: List[List[Int]]): List[List[Int]] = {
    move match {
      case Failure(_) =>
        println(s"${Console.RED}INVALID INPUT${Console.RESET}")
        currentPosition
      case Success(move) if !validInput(move) =>
        println(s"${Console.RED}INVALID INPUT${Console.RESET}")
        currentPosition
      case Success(move) if !validMove(move, updateBoard(move, currentPosition), startPosition) =>
        println(s"${Console.RED}INVALID MOVE${Console.RESET}")
        currentPosition
      case Success(move) =>
        println(s"${Console.GREEN}OK${Console.RESET}")
        updateBoard(move, currentPosition)
    }
  }

  @tailrec
  def gameLoop(player: Player, currentPosition: List[List[Int]], startPosition: List[List[Int]]): Unit = {
    print(renderSudoku(currentPosition))
    checkGameStatus(currentPosition) match {
      case End =>
        print(s"${Console.GREEN}You win!${Console.RESET}")
      case InProcess =>
        print("Enter row, column and number: [0-8] [1-9] [1-9]\n")
        gameLoop(player, nextState(player.move, currentPosition, startPosition), startPosition)
    }
  }
}