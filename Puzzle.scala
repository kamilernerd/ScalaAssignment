import scala.Byte.MaxValue
import scala.language.postfixOps

// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  val solutionArray = solution.split("\n").toList;

  var emptyRow = "____";

  var i = 0;
  for (i <- 1 until solutionArray.length) {
    var row = solutionArray(i);



    val pattern = row match {
      case row if emptyRow.equals(row) => "...."
      case _ => row
    }

    println(pattern);
  }

  def checkMove(pos: Integer, row: String, isLastElement: Boolean): Boolean = {
    if (pos <= row.length || pos >= 0) {
      if (isLastElement) {
        return false;
      }

      if (!row(pos).equals('X') ||
          row(pos).isNaN ||
          !row(pos).equals('*')
      ) {
        if (pos == (row.length - 1)) {
          checkMove(pos + 1, row, isLastElement = true);
        } else {
          checkMove(pos + 1, row, isLastElement = false);
        }
      } else {
        return false;
      }
    }
  }

  def walk(row: String): Unit = {
    var i = 0;
    for (i <- 0 until row.length) {
      if (row(i).isDigit) {
        if (checkMove(i, row, false)) {
          row(i + 1) = '*';
        }
      }
    }
  }

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}
