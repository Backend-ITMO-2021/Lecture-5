package ru.ifmo.backend_2021

class Computer extends Player {
  private var moves = List((0,0,0)).empty
  override def makeStep(field: List[List[Int]]): Array[Int] = {
    if (moves.isEmpty)
      findSolution(field)
    if (moves.isEmpty) {
      return null
    }
    val nextStep = moves.head
    moves = moves.drop(1)
    Array(nextStep._1, nextStep._2, nextStep._3)
  }

  private def findSolution(field: List[List[Int]]): List[List[Int]] = {
    val pos = findEmpty(field)
    if (pos == null) return field
    val possibleMoves = getPossibleNumbers(field, pos._1, pos._2)
    for (number <- possibleMoves) {
      val resultField = findSolution(field.updated(pos._1, field(pos._1).updated(pos._2, number)))
      if (resultField != null) {
        moves = (pos._1, pos._2 + 1, number) +: moves
        return field
      }
    }
    null
  }

  private def findEmpty(field: List[List[Int]]): (Int, Int) = {
    Range(0, 9).foreach(i => Range(0,9).foreach(j =>
      if (field(i)(j) == 0) return (i, j)))
    null
  }

  private def getPossibleNumbers(field: List[List[Int]], row:Int, col:Int): List[Int] = {
    val curField = field.transpose
    val startRow = row / 3 * 3
    val startCol = col /3 * 3
    val numbers = field(row).distinct ++ curField(col).distinct ++
      field.slice(startRow, startRow + 3).transpose.slice(startCol, startCol + 3).flatten.distinct
    List(1, 2, 3, 4, 5, 6, 7, 8, 9) diff numbers.distinct
  }
}
