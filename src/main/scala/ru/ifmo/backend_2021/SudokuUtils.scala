package ru.ifmo.backend_2021


object SudokuUtils {
  def main(args: Array[String]) = {
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
    print(isValidSudoku(sudoku))
    val action: String = ""
    while (action != "3") {
      print(("Выберете действие:\n 1. Ввести значения\n 2. Показать заполненную судоку\n 3. Закончить игру\n"))
      val action: String = scala.io.StdIn.readLine()
      action match {
        case "1" => playSudoku(sudoku)
        case "2" => print(renderSudoku(sudoku))
        case "3" => print(renderSudoku(sudoku))
      }
    }
  }

  def playSudoku(sudoku: List[List[Int]]): Unit = {
    print(renderSudoku(sudoku))
    print("Введите значения координат и само значение через пробел(x, y, value):\nВведите 'q', чтобы закончить игру.\n")
    val input: String = scala.io.StdIn.readLine()
    if (input == "q") {
      return false
    }
    val x: Int = input.split(" ")(0).toInt
    val y: Int = input.split(" ")(1).toInt - 1
    val value: Int = input.split(" ")(2).toInt
    val newSudoku = {
      if (sudoku(x)(y) == 0)
        sudoku.patch(x, Seq(sudoku(x).patch(y, Seq(value), 1)), 1)
      else {
        sudoku
      }
    }
    val testSudoku = {
      if (isValidSudoku(newSudoku)) {
        newSudoku
      } else {
        showEx(newSudoku)
      }
    }

    playSudoku(newSudoku);
  }

  def showEx(rawSudoku: List[List[Int]]): Unit = {

  }

  def checkSquad(rawSudoku: List[List[Int]], s: Int, c: Int): Boolean = {
    var nums: Set[Int] = Set();
    for (i <- s to s + 2) {
      for (j <- c to c + 2) {
        if (!nums(rawSudoku(i)(j))) {
          if (rawSudoku(i)(j) != 0)
            nums += rawSudoku(i)(j);
        } else {
          return false
        }
      }
    }
    return true;
  }

  def checkColumn(rawSudoku: List[List[Int]], r: Int, c: Int): Boolean = {
    var nums: Set[Int] = Set();
    for (i <- c to 8) {
      if (!nums(rawSudoku(r)(i))) {
        if (rawSudoku(r)(i) != 0)
          nums += rawSudoku(r)(i);
      } else {
        return false
      }
    }
    return true
  }

  ///indexes:Array[Array[Int]]=Array(Array())
  def checkRows(rawSudoku: List[List[Int]], r: Int, c: Int): Boolean = {
    var nums: Set[Int] = Set();
    for (i <- r to 8) {
      if (!nums(rawSudoku(i)(c))) {
        if (rawSudoku(i)(c) != 0)
          nums += rawSudoku(i)(c);
      } else {
        return false
      }
    }
    return true
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    //    var indexes:Array[Array[Int]]=Array()
    //квадраты
    val indexs: List[Int] = List(0, 3, 6)
    indexs.foreach(index => {
      indexs.foreach(index2 => {
        if (!checkSquad(rawSudoku, index, index2))
          return false
      })
    })
    for (i <- 0 to 8) {
      if (!checkColumn(rawSudoku, i, 0))
        return false
      if (!checkRows(rawSudoku, 0, i))
        return false
    }
    return true
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    def isZero(num: Int): String = {
      if (num == 0) {
        return " ";
      } else {
        return f"$num"
      }
    }
    def addRow(row: List[Int], rowIndex: Int): String = {row.zipWithIndex.map(zip => {
      val column = zip._2
      val value =isZero(zip._1)
      if((column+1)%3==0)
        f"${value} |"
      else
        f"${value}"
    }).mkString(" ")
    }
    val line = "--+-------+-------+-------+"
    grid.zipWithIndex.map(str => {
      if (str._2 == 0)
        f"\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n${line}\n${str._2} | ${addRow(str._1, str._2)}\n"
      else if ((str._2+1) % 3 == 0 || (str._2 == grid.length - 1))
        f"${str._2} | ${addRow(str._1, str._2)}\n$line\n"
      else
        f"${str._2} | ${addRow(str._1, str._2)}\n"
    }).mkString("")
  }
}