package ru.ifmo.backend_2021


object SudokuUtils {
  def isValidSudoku(grid: List[List[Int]]): Boolean = {
    findMistakes(grid) == Set()
  }

  def findZeros(grid: List[List[Int]]): Set[(Int, Int)] =
    grid.zipWithIndex.flatMap{
      case (x, i) =>
        x.zipWithIndex
          .filter{ _._1 == 0 }
          .map{ case (_, j) => (i, j)
          }
    }.toSet


  def findMistakes(grid: List[List[Int]]): Set[(Int, Int)] = {
    val boxes = grid.zipWithIndex.map { case (x, i) =>
      x.zipWithIndex.map { case (_, j) =>
        grid((i / 3) * 3 + j / 3)((i % 3) * 3 + j % 3)
      }
    }
    var all_mistakes = Set.empty[(Int, Int)]

    grid.zipWithIndex.foreach{
      case (row, i) => {
        val mistakes_j = findMistakesList(row)
        if (mistakes_j != List()) all_mistakes = mistakes_j.map{ j => (i, j) }.toSet
      }
    }

    grid.transpose.zipWithIndex.foreach {
      case (col, j) => {
        val mistakes_i = findMistakesList(col)
        if (mistakes_i != List()) all_mistakes = all_mistakes.union{ mistakes_i.map{ i => (i, j) }.toSet }
      }
    }

    boxes.zipWithIndex.foreach{
      case (box, box_index) => {
        val mistakes_box = findMistakesList(box)
        if (mistakes_box != List())
          all_mistakes = all_mistakes.union{
            mistakes_box.map {
              box_x =>
                (
                  (box_index / 3) * 3 + box_x / 3,
                  (box_index % 3) * 3 + box_x % 3
                )
            }.toSet
          }
      }
    }
    all_mistakes
  }

  private def findMistakesList(list: List[Int]): List[Int] =
    list
      .zipWithIndex
      .filter{ _._1 != 0 }
      .groupBy{ _._1 }
      .filter{ _._2.size > 1 }
      .flatMap{
        _._2.map{ _._2 }
      }
      .toList

  private def gridTransformIntToString(grid: List[List[Int]], added_numbers_coordinates: Set[(Int, Int)] = Set.empty[(Int, Int)], mistakes: Set[(Int, Int)] = Set.empty[(Int, Int)]): List[List[String]] =
    grid
      .zipWithIndex
      .map{
        case (row, i) =>
          row
            .zipWithIndex
            .map{
              case (0, _) => " "
              case (elem, j) if mistakes.contains((i, j)) => Console.RED + elem.toString + Console.RESET
              case (elem, j) if added_numbers_coordinates.contains((i, j)) => Console.YELLOW + elem.toString + Console.RESET
              case (elem, _) => elem.toString
            }
      }

  def renderSudoku(grid: List[List[Int]], added_numbers_coordinates: Set[(Int, Int)] = Set.empty[(Int, Int)], mistakes: Set[(Int, Int)] = Set.empty[(Int, Int)], isWin: Boolean = false): String = {
    if (isWin)
      renderSudokuColored(
        gridTransformIntToString(grid),
        isWin = true
      )
    else
      renderSudokuColored(
        gridTransformIntToString(
          grid,
          added_numbers_coordinates = added_numbers_coordinates,
          mistakes = mistakes
        )
      )
  }

  private def renderSudokuColored(grid: List[List[String]], isWin: Boolean = false): String = {
    val separator = "\n--+-------+-------+-------+\n"
    val col_numbers = "\n  | 1 2 3 | 4 5 6 | 7 8 9 |"
    val grid_divided = grid.map{ row => row.sliding(3, 3).toList }.sliding(3, 3).toList
    val sudokuRows = grid_divided.zipWithIndex.map{
      case (three_rows, i) => three_rows.zipWithIndex.map{
        case (row, j) =>
          (i * 3 + j).toString + " | " + row.map{
            x => x.mkString(" ")
          }.mkString(" | ") + " |"
      }.mkString("\n")
    }.mkString(separator)
    if (isWin)
      s"""${Console.GREEN}$col_numbers$separator$sudokuRows$separator${Console.RESET}"""
    else
      s"""$col_numbers$separator$sudokuRows$separator"""
  }

  def isWin(grid: List[List[Int]]): Boolean = {
    isValidSudoku(grid) && !grid.exists(row => row.contains(0))
  }
}