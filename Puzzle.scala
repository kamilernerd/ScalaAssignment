import scala.collection.mutable.ArrayBuffer

// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  val solutionArray = solution.split("\n").toList;

  var i = 0;
  for (i <- 0 until solutionArray.length) {
    walk(i, solutionArray(i).toCharArray);
  }

  def walk(yVal: Integer, row: Array[Char]): Unit = {
    var i = 0;

    while (i < row.length) {
      val x = i;
      val y = yVal;

      val next = y + 1
      val prev = y - 1

      val prevRow = if (prev < 0) solutionArray.head.toCharArray else solutionArray(prev).toCharArray;
      val nextRow = if (next < solutionArray.size) solutionArray(next).toCharArray else solutionArray(y).toCharArray

      row(i).charValue() match {
        case '0' => zeroMatch(x, y, row, prevRow, nextRow)
        case '1' => oneMatch(x, y)
        case '2' => twoMatch(x, y)
        case '3' => threeMatch(x, y)
        case '4' => fourMatch(x, y)
        case 'X' => fourMatch(x, y)
        case '*' => fourMatch(x, y)
        case '_' => fourMatch(x, y)
        case _  => row
      }

      i += 1;
    }

    println(String.valueOf(row));
  }

  def zeroMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {
    // Case 1 - Check left
    if (row(x - 1).charValue() == '_') {
      row.update(x - 1, '~');
    }

    // Case 2 - Check right
    if (row(x + 1).charValue() == '_') {
      row.update(x + 1, '~');
    }

    // Case 4 - Check above
    if (prevRow(x).charValue() == '_') {
      prevRow.update(x, '~');
    }

    // Case 6 - Check below
    if (nextRow(x).charValue() == '_') {
      nextRow.update(x, '~');
    }
  }

  def oneMatch(x: Integer, y: Integer): Unit = {

  }

  def twoMatch(x: Integer, y: Integer): Unit = {

  }

  def threeMatch(x: Integer, y: Integer): Unit = {

  }

  def fourMatch(x: Integer, y: Integer): Unit = {

  }

  /*
  def checkMoveForward(pos: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]): Boolean = {
    if (pos == row.length - 1) {
      return false;
    }

    if (row(pos).charValue() == '*' || row(pos).charValue() == '0' || row(pos).charValue().isDigit) {
      return false;
    }

    // Check the next position
    if (row(pos).charValue() == '_') {
      // Check the position after that
      return checkMoveForward(pos + 1, row, prevRow, nextRow);
    }
    return true;
  }

  def checkMoveBackward(pos: Integer, row: Array[Char]): Boolean = {
    if (pos == 0) {
      return false;
    }

    if (row(pos).charValue() == '*' || row(pos).charValue() == '0' || row(pos - 1).charValue().isDigit) {
      return false;
    }

    // Check the previous position
    if (row(pos).charValue() == '_') {
      // Check the position after that
      return checkMoveBackward(pos - 1, row);
    }
    return true;
  }
  */

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}
