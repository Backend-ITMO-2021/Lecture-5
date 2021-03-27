import org.scalatest.funsuite.AnyFunSuite
import ru.ifmo.backend_2021.SudokuUtils

class SudokuTest extends AnyFunSuite {
  private lazy val correctSudoku =
    List(
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

  private lazy val incorrectSudoku =
    List(
      List(3, 1, 6, 5, 7, 8, 4, 9, 3),
      List(5, 2, 9, 1, 3, 4, 7, 6, 8),
      List(4, 8, 7, 6, 2, 9, 5, 3, 1),

      List(2, 6, 3, 0, 1, 0, 0, 8, 0),
      List(9, 7, 4, 8, 6, 3, 0, 0, 5),
      List(8, 5, 1, 0, 9, 0, 6, 0, 0),

      List(1, 3, 0, 0, 0, 0, 2, 5, 0),
      List(0, 0, 0, 0, 0, 0, 0, 7, 4),
      List(0, 0, 5, 2, 0, 6, 3, 0, 0)
    )

  private lazy val renderSudokuResult =
    """
  | 1 2 3 | 4 5 6 | 7 8 9 |
--+-------+-------+-------+
0 | 3 1 6 | 5 7 8 | 4 9 2 |
1 | 5 2 9 | 1 3 4 | 7 6 8 |
2 | 4 8 7 | 6 2 9 | 5 3 1 |
--+-------+-------+-------+
3 | 2 6 3 |   1   |   8   |
4 | 9 7 4 | 8 6 3 |     5 |
5 | 8 5 1 |   9   | 6     |
--+-------+-------+-------+
6 | 1 3   |       | 2 5   |
7 |       |       |   7 4 |
8 |     5 | 2   6 | 3     |
--+-------+-------+-------+
"""

  test("isValidSudoku") {
    assert(SudokuUtils.isValidSudoku(correctSudoku))
    assert(!SudokuUtils.isValidSudoku(incorrectSudoku))
  }

  test("renderSudoku") {
    assertResult(renderSudokuResult)(SudokuUtils.renderSudoku(correctSudoku))
  }
}
