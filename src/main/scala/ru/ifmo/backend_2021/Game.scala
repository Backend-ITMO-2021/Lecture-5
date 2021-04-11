package ru.ifmo.backend_2021

/**
 * @author Vladimir Goncharov
 * @created 11.04.2021
 */
class Game(var field: List[List[Int]], player: Player) {

  private def isGameOver() = {
    SudokuUtils.isFinishedSudoku(field);
  }

  def start(): Unit = {
    while (!isGameOver()) {
      val turn = player.nextTurn(field);
      if (isValidTurn(turn)) {
        val tempField = field.updated(turn._1, field(turn._1).updated(turn._2, turn._3))
        if (SudokuUtils.isValidSudoku(tempField))
          field = tempField
        else
          player.wrongTurn("Your turn doesn't suit the field", field)
      } else {
        player.wrongTurn("Your turn is not correct. Check if indexes are -1<i<9 and value 0<v<10", field)
      }
    }
    end()
  }

  private def isValidTurn(turn: (Int, Int, Int)): Boolean = {
    turn._1 >= 0 && turn._1 < 9 && turn._2 >= 0 && turn._2 < 9 && turn._3 > 0 && turn._3 < 10
  }

  private def end(): Unit = {
    println("Result: ")
    println(SudokuUtils.renderSudoku(field))
  }
}
