package ru.ifmo.backend_2021

object SudokuUtils {
  def main(args: Array[String]): Unit = {
    val sudoku = List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 2),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),

      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),

      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )
    print(renderSudoku(sudoku))
    while (true) {
      print(("Выберете действие:\n 1. Ввести значения\n 2. Показать заполненную судоку\n 3. Закончить работу\n"))
      val action: String = scala.io.StdIn.readLine()
      action match {
        case "1" => playSudoku(List(sudoku, sudoku))
        case "2" => print(renderSudoku(backtracking(sudoku)))
        case "3" => sys.exit
      }
    }

  }

  def playSudoku(sudoku: List[List[List[Int]]]): Boolean = {
    print("Введите значения координат и само значение через пробел(x, y, value):\nВведите 'quit', чтобы закончить игру.\n")
    val readVal: String = scala.io.StdIn.readLine()
    if (readVal == "quit") {
      return false
    }
    val inValue: Array[String] = readVal.split(" ")
    val x = inValue(1).toInt
    val y = inValue(0).toInt
    val value = inValue(2).toInt
    val new_sudoku = add_val(sudoku(0), x, y, value)
    if (!isValidSudoku(new_sudoku)) {
      print("Была найдена ошибка:\n")
    }
    val render_sudoku = craeteRenderSudoku(new_sudoku)
    print(renderSudoku(render_sudoku))
    playSudoku(List(new_sudoku, render_sudoku))
  }
  def changeDuplicateSquare(newRawSudoku: List[List[Int]], value: Int, points: List[Int], squareID: Int): List[List[Int]] = {
    if (points.isEmpty) {
      return newRawSudoku
    }
    val row = (squareID/3)*3 + points.head / 3
    val col = (squareID%3)*3 + points.head % 3
    if (newRawSudoku(row)(col) == value) {
      val newSudoku = newRawSudoku.patch(row, Seq(newRawSudoku(row).patch(col, Seq(-value), 1)), 1)
      return changeDuplicateSquare(newSudoku, value, points.patch(0, Nil, 1), squareID)
    } else {
      changeDuplicateSquare(newRawSudoku, value, points.patch(0, Nil, 1), squareID)
    }
  }

  def findDuplicateSquare(rawSudoku: List[List[Int]], newRawSudoku: List[List[Int]], value: List[Int], square: List[Int], squareID: Int): List[List[Int]] = {
    if (value.isEmpty) {
      return newRawSudoku
    }
    if (square.count(_ == value.head) > 1) {
      val newSudoku = changeDuplicateSquare(newRawSudoku, value.head, Range(0, 9).toList, squareID)
      return findDuplicateSquare(rawSudoku, newSudoku, value.patch(0, Nil, 1), square, squareID)
    } else {
      return findDuplicateSquare(rawSudoku, newRawSudoku, value.patch(0, Nil, 1), square, squareID)
    }
  }

  def makeSquare(rawSudoku: List[List[Int]], newRawSudoku: List[List[Int]], squares: List[Int]): List[List[Int]] = {
    if (squares.isEmpty) {
      return newRawSudoku
    }
    val square = Range(0, 9).map(j => rawSudoku((squares.head/3)*3 + j/3)((squares.head%3)*3+j%3))
    val newSudoku = findDuplicateSquare(rawSudoku, newRawSudoku, Range(1, 10).toList, square.toList, squares.head)
    return makeSquare(rawSudoku, newSudoku, squares.patch(0, Nil, 1))

  }
  def changeDuplicateString(newRawSudoku: List[List[Int]], value: Int, col: List[Int], row: Int): List[List[Int]] = {
    if (col.isEmpty) {
      return newRawSudoku
    }
    if (newRawSudoku(row)(col.head) == value) {
      val newSudoku = newRawSudoku.patch(row, Seq(newRawSudoku(row).patch(col.head, Seq(-value), 1)), 1)
      return changeDuplicateString(newSudoku, value, col.patch(0, Nil, 1), row)
    } else {
      changeDuplicateString(newRawSudoku, value, col.patch(0, Nil, 1), row)
    }
  }
  def findDuplicateString(rawSudoku: List[List[Int]], newRawSudoku: List[List[Int]], value: List[Int], string: List[Int], row: Int): List[List[Int]] = {
    if (value.isEmpty) {
      return newRawSudoku
    }
    if (string.count(_ == value.head) > 1) {
      val newSudoku = changeDuplicateString(newRawSudoku, value.head, Range(0, 9).toList, row)
      return findDuplicateString(rawSudoku, newSudoku, value.patch(0, Nil, 1), string, row)
    } else {
      return findDuplicateString(rawSudoku, newRawSudoku, value.patch(0, Nil, 1), string, row)
    }
  }
  def makeString(rawSudoku: List[List[Int]], newRawSudoku: List[List[Int]], strings: List[Int]): List[List[Int]] = {
    if (strings.isEmpty) {
      return newRawSudoku
    }
    val newSudoku = findDuplicateString(rawSudoku, newRawSudoku, Range(1, 10).toList, rawSudoku(strings.head), strings.head)
    return makeString(rawSudoku, newSudoku, strings.patch(0, Nil, 1))

  }

  def changeDuplicateColumn(newRawSudoku: List[List[Int]], value: Int, row: List[Int], col: Int): List[List[Int]] = {
    if (row.isEmpty) {
      return newRawSudoku
    }
    if (newRawSudoku(row.head)(col) == value) {
      val newSudoku = newRawSudoku.patch(row.head, Seq(newRawSudoku(row.head).patch(col, Seq(-value), 1)), 1)
      return changeDuplicateColumn(newSudoku, value, row.patch(0, Nil, 1), col)
    } else {
      changeDuplicateColumn(newRawSudoku, value, row.patch(0, Nil, 1), col)
    }
  }
  def findDuplicateColumn(rawSudoku: List[List[Int]], newRawSudoku: List[List[Int]], value: List[Int], column: List[Int], col: Int): List[List[Int]] = {
    if (value.isEmpty) {
      return newRawSudoku
    }
    if (column.count(_ == value.head) > 1) {
      val newSudoku = changeDuplicateColumn(newRawSudoku, value.head, Range(0, 9).toList, col)
      return findDuplicateColumn(rawSudoku, newSudoku, value.patch(0, Nil, 1), column, col)
    } else {
      return findDuplicateColumn(rawSudoku, newRawSudoku, value.patch(0, Nil, 1), column, col)
    }
  }
  def makeColumn(rawSudoku: List[List[Int]], newRawSudoku: List[List[Int]], columns: List[Int]): List[List[Int]] = {
    if (columns.isEmpty) {
      return newRawSudoku
    }
    val newSudoku = findDuplicateColumn(rawSudoku, newRawSudoku, Range(1, 10).toList, Range(0, 9).map(rawSudoku(_)(columns.head)).toList, columns.head)
    return makeColumn(rawSudoku, newSudoku, columns.patch(0, Nil, 1))

  }

  def craeteRenderSudoku(rawSudoku: List[List[Int]]): List[List[Int]] = {
    // квадраты
    val squareSudoku = makeSquare(rawSudoku, rawSudoku, Range(0, 9).toList)
    // строки
    val stringSudoku = makeString(rawSudoku, squareSudoku, Range(0, 9).toList)
    // столбцы
    val columnSudoku = makeColumn(rawSudoku, stringSudoku, Range(0, 9).toList)
    return columnSudoku
  }
  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    // квадраты
    for (i <- 0 to 8) {
      val square = Range(0, 9).map(j => rawSudoku((i/3)*3 + j/3)((i%3)*3+j%3)).filter(_ > 0)
      if (!(square == square.distinct)) {
        return false
      }
    }
    // строки
    for (i <- 0 to 8) {
      val row = Range(0, 9).map(rawSudoku(i)).filter(_ > 0)
      if (!(row == row.distinct)) {
        return false
      }
    }
    // столбцы
    for (i <- 0 to 8) {
      val col = Range(0, 9).map(rawSudoku(_)(i)).filter(_ > 0)
      if (!(col == col.distinct)) {
        return false
      }
    }

    return true
  }
  def add_val(grid: List[List[Int]], row: Int, col: Int, value: Int): List[List[Int]] = {
    return grid.patch(row, Seq(grid(row).patch(col, Seq(value), 1)), 1)

  }
  def backtracking(grid: List[List[Int]]): List[List[Int]] = {
    for (i: Int <- 0 to 8) {
      for (j: Int <- 0 to 8) {
        if (grid(i)(j) == 0) {
          for (value: Int <- 1 to 9) {
            val new_grid = add_val(grid, i, j, value)
            if (isValidSudoku(new_grid)) {
              val result = backtracking(new_grid)
              if (result != List(List(0))) {
                return result
              } else {
                return List(List(0))
              }
            }
          }
        }
      }
    }
    return grid
  }
  // newRawSudoku(row).patch(col.head, Seq(-value), 1)
  def addValuesFromString(grid: List[List[Int]], result: List[String], stringID: Int, cells: List[Int]): List[String] = {
    if (cells.isEmpty) {
      return result
    }
    if (cells.head%3 == 0) {
      grid(stringID)(cells.head) match {
        // ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨
        case 0 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " +" " + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -1 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "①" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -2 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "②" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -3 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "③" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -4 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "④" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -5 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "⑤" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -6 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "⑥" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -7 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "⑦" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -8 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "⑧" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -9 => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + "⑨" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case _ => return addValuesFromString(grid, result.patch(result.length-1, Seq("| " + grid(stringID)(cells.head).toString + " "), 0), stringID, cells.patch(0, Nil, 1))
      }
    } else {
      grid(stringID)(cells.head) match {
        // ① ② ③ ④ ⑤ ⑥ ⑦ ⑧ ⑨
        case 0 => return addValuesFromString(grid, result.patch(result.length-1, Seq(" " + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -1 => return addValuesFromString(grid, result.patch(result.length-1, Seq("①" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -2 => return addValuesFromString(grid, result.patch(result.length-1, Seq("②" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -3 => return addValuesFromString(grid, result.patch(result.length-1, Seq("③" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -4 => return addValuesFromString(grid, result.patch(result.length-1, Seq("④" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -5 => return addValuesFromString(grid, result.patch(result.length-1, Seq("⑤" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -6 => return addValuesFromString(grid, result.patch(result.length-1, Seq("⑥" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -7 => return addValuesFromString(grid, result.patch(result.length-1, Seq("⑦" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -8 => return addValuesFromString(grid, result.patch(result.length-1, Seq("⑧" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case -9 => return addValuesFromString(grid, result.patch(result.length-1, Seq("⑨" + " "), 0), stringID, cells.patch(0, Nil, 1))
        case _ => return addValuesFromString(grid, result.patch(result.length-1, Seq(grid(stringID)(cells.head).toString + " "), 0), stringID, cells.patch(0, Nil, 1))
      }
    }
  }
  def addValues(grid: List[List[Int]], result: List[String], strings: List[Int]): List[String] = {
    if (strings.isEmpty) {
      return result
    }
    if (strings.head%3 == 0) {
      val new_result = addValuesFromString(grid, result.patch(result.length-1, Seq("|\n--+-------+-------+-------+"), 0), strings.head, Range(0, 9).toList)
      return addValues(grid, new_result.patch(result.length, Seq("\n" + strings.head + " "), 0), strings.patch(0, Nil, 1))
    } else {
      val new_result = addValuesFromString(grid, result, strings.head, Range(0, 9).toList)
      return addValues(grid, new_result.patch(result.length-1, Seq("|\n" + strings.head + " "), 0), strings.patch(0, Nil, 1))
    }
  }
  def renderSudoku(grid: List[List[Int]]): String = {
    val result = List("\n  | 1 2 3 | 4 5 6 | 7 8 9 ", "|\n--+-------+-------+-------+\n")
    val new_result = addValues(grid, result, Range(0, 9).toList)
    return new_result.mkString
  }
}


