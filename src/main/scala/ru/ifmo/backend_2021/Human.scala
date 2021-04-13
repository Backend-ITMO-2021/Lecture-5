package ru.ifmo.backend_2021

import scala.io.StdIn.readLine

class Human extends Player {
  override def makeStep(field: List[List[Int]]): Array[Int] = {
    println("Введите через пробел строку, столбец и значение")
    val input = readLine().split(" ")
    if (!correctInput(input)){
      println(Console.RED + "Некорректный формат ввода" + Console.RESET)
      makeStep(field)
    }
    input.map(_.toInt)
  }

  def correctInput(input: Array[String]): Boolean = {
    input.length == 3 && input(0).toInt > 0 && input(0).toInt < 10 && input(1).toInt >
      0 && input(1).toInt < 10 && input(2).toInt > 0 && input(2).toInt < 10
  }
}
