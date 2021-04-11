package ru.ifmo.backend_2021

/**
 * @author Vladimir Goncharov
 * @created 11.04.2021
 */
trait Player {
  def nextTurn(field: List[List[Int]]): (Int, Int, Int)
  def wrongTurn(message: String, field: List[List[Int]])
}
