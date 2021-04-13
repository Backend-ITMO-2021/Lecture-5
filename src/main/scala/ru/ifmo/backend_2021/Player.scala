package ru.ifmo.backend_2021

trait Player {
  def makeStep(field: List[List[Int]]): Array[Int]
}
