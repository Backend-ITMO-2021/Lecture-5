package ru.ifmo.backend_2021

object CustomExceptions {
  final case class SudokuOccupiedException(private val message: String = "Incorrect step, this square is already occupied, try again",
                                   private val cause: Throwable = None.orNull)
    extends Exception(message, cause)

  final case class SudokuRulesException(private val message: String = "Incorrect step, can't put this number in the square, try again",
                                           private val cause: Throwable = None.orNull)
    extends Exception(message, cause)
}
