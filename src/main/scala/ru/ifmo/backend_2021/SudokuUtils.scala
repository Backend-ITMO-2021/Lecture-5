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
    val valid = isValidSudoku(newSudoku)
    val testSudoku = newSudoku

    if(!valid){
      print("не валидно")
      print(showEx(testSudoku, x,y,value))
      playSudoku(sudoku);
    }
    playSudoku(newSudoku);
  }

  def showNumsSquad(testSudoku: List[List[Int]], s: Int, c: Int){

  }
  def showEx(rawSudoku: List[List[Int]], x:Int, y:Int, value:Int): String = {
    val symbols = List("①", "②",  "③", "④", "⑤", "⑥", "⑦", "⑧", "⑨")
    def isZero(num: Int): String = {
      if (num == 0)
        return " "
      return f"$num"
    }
    def getSq(row: List[Int], x:Int, y:Int): Any ={
      if(y < 3 && x < 3){
        return Range(0, 9).map(j => getSquare(rawSudoku,0,j)).filter(_ > 0) //0
      }
      if(y > 2 && y < 6 && x<3){
        return Range(0, 9).map(j => getSquare(rawSudoku,1,j)).filter(_ > 0) //1
      }
      if(y > 5 && y < 9 && x<3){
        return Range(0, 9).map(j => getSquare(rawSudoku,2,j)).filter(_ > 0) //2
      }
      if(y < 3 && x < 6 && x>2){
        return Range(0, 9).map(j => getSquare(rawSudoku,3,j)).filter(_ > 0) //3
      }
      if(y > 2 && y < 6 && x>2 && x<6){
        return Range(0, 9).map(j => getSquare(rawSudoku,4,j)).filter(_ > 0) //4
      }
      if(y > 5 && y < 9 && x>2 && x<6){
        return Range(0, 9).map(j => getSquare(rawSudoku,5,j)).filter(_ > 0) //5
      }
      if(y < 3 && x>5){
        return Range(0, 9).map(j => getSquare(rawSudoku,6,j)).filter(_ > 0) //6
      }
      if(y > 3 && y < 6 && x>5){
        return Range(0, 9).map(j => getSquare(rawSudoku,7,j)).filter(_ > 0) //7
      }
      if(y > 6 && y < 10 && x>5){
        return Range(0, 9).map(j => getSquare(rawSudoku,8,j)).filter(_ > 0) //8
      }
    }

    def addRow(row: List[Int], rowIndex: Int): String = {
      row.zipWithIndex.map(zip => {
        val sq = getSq(row, x,y)
        val sq1 = getSq(row, rowIndex,zip._2)
        if(sq == sq1 && (zip._2 + 1) % 3 == 0 && rawSudoku(x)(y) == zip._1) {
          f"${symbols(value-1)} |"
        }else if(sq == sq1 && rawSudoku(x)(y) == zip._1){
          f"${symbols(value-1)}"
        }else
        if ((zip._2 + 1) % 3 == 0 && (zip._2==y && zip._1 ==value || rowIndex==x && zip._1 ==value)) {
          f"${symbols(value-1)} |"
        } else if ((zip._2 + 1) % 3 == 0) {
          f"${isZero(zip._1)} |"
        } else if (zip._2==y && zip._1 ==value || rowIndex==x && zip._1 ==value){
          f"${symbols(value-1)}"
        }else{
          f"${isZero(zip._1)}"
        }
      }).mkString(" ")
    }
    val line = "--+-------+-------+-------+"

    rawSudoku.zipWithIndex.map(str => {
      if (str._2 == 0)
        f"\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n${line}\n${str._2} | ${addRow(str._1, str._2)}\n"
      else if ((str._2 + 1) % 3 == 0 || (str._2 == rawSudoku.length - 1))
        f"${str._2} | ${addRow(str._1, str._2)}\n$line\n"
      else
        f"${str._2} | ${addRow(str._1, str._2)}\n"
    }).mkString("")
  }

  def getSquare(rawSudoku: List[List[Int]], i:Int, j:Int): Int ={
    return rawSudoku((i / 3) * 3 + j / 3)((i % 3) * 3 + j % 3)
  }

  def isValidSudoku(rawSudoku: List[List[Int]]): Boolean = {
    !Range(0, 9).exists{ i =>
      val row = Range(0, 9).map(j => rawSudoku(i)(j)).filter(_ > 0)
      val col = Range(0, 9).map(j => rawSudoku(j)(i)).filter(_ > 0)
      val square = Range(0, 9).map(j => getSquare(rawSudoku,i,j)).filter(_ > 0)
      row.distinct.length != row.length || col.distinct.length != col.length || square.distinct.length != square.length
    }
  }

  def renderSudoku(grid: List[List[Int]]): String = {
    def isZero(num: Int): String = {
      if (num == 0)
        return " "
      return f"$num"
    }
    def addRow(row: List[Int], rowIndex: Int): String = {
      row.zipWithIndex.map(zip => {
        if ((zip._2 + 1) % 3 == 0)
          f"${isZero(zip._1)} |"
        else
          f"${isZero(zip._1)}"
      }).mkString(" ")
    }
    val line = "--+-------+-------+-------+"
    grid.zipWithIndex.map(str => {
      if (str._2 == 0)
        f"\n  | 1 2 3 | 4 5 6 | 7 8 9 |\n${line}\n${str._2} | ${addRow(str._1, str._2)}\n"
      else if ((str._2 + 1) % 3 == 0 || (str._2 == grid.length - 1))
        f"${str._2} | ${addRow(str._1, str._2)}\n$line\n"
      else
        f"${str._2} | ${addRow(str._1, str._2)}\n"
    }).mkString("")
  }
}