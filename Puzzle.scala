import scala.Byte.MaxValue
import scala.language.postfixOps

// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  val solutionArray = solution.split("\n").toList;

  var i = 0;
  for (i <- 1 until solutionArray.length) {
    walk(solutionArray(i).toCharArray);
  }

  def walk(row: Array[Char]): Unit = {
    var i = 0;
    while (i < row.length) {
      if (row(i).isDigit) {
        if (row(i).charValue() == '1') {

        } else if (row(i).charValue() == '2') {
          if (checkMoveForward(i, row)) {
            row(i + 1) = '*'; 
          }

          if (checkMoveBackward(i, row)) {
            row(i - 1) = '*';
          }
        }

      } else if (row(i).charValue() == 'X') {
        // Current element is an X
      } else if (row(i).charValue() == '*') {
        // Current element is a light
      } else if (row(i).charValue() == '_') {
        // Current element is empty
      }

      i += 1;
    }

    println(String.valueOf(row));
    return row;
  }

  def checkMoveForward(pos: Integer, row: Array[Char]): Boolean = {
    if (pos == row.length - 1) {
      return false;
    }

    if (row(pos).charValue() == '*' || row(pos).charValue() == '0') {
      return false;
    }

    // Check the next position
    if (row(pos).charValue() == '_') {
      // Check the position after that
      return checkMoveForward(pos + 1, row);
    }
    return true;
  }

  def checkMoveBackward(pos: Integer, row: Array[Char]): Boolean = {
    if (pos == 0) {
      return false;
    }

    if (row(pos).charValue() == '*' || row(pos).charValue() == '0') {
      return false;
    }

    // Check the previous position
    if (row(pos).charValue() == '_') {
      // Check the position after that
      return checkMoveForward(pos - 1, row);
    }
    return true;
  }

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}
