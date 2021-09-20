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

  def walk(y: Number, row: Array[Char]): Unit = {
    var i = 0;
    while (i < row.length) {
      val x = i;
      val y = y;

      row(i).charValue() match {
        case '0' => zeroMatch(x, y)
        case '1' => oneMatch(x, y)
        case '2' => twoMatch(x, y)
        case '3' => threeMatch(x, y)
        case '4' => fourMatch(x, y)
        case 'X' => fourMatch(x, y)
        case '*' => fourMatch(x, y)
        case '_' => fourMatch(x, y)
      }

      i += 1;
    }

    println(String.valueOf(row));
    return row;
  }

  def zeroMatch(x: Integer, y: Integer): Unit = {
    val row = solutionArray(y).toCharArray
    val nextRow = solutionArray(y + 1).toCharArray
    val prevRow = solutionArray(y - 1).toCharArray

    // Case 1
    if () {

    }
  }

  def oneMatch(x: Integer, y: Integer): Unit = {
    val row = solutionArray(y).toCharArray
    val nextRow = solutionArray(y + 1).toCharArray
    val prevRow = solutionArray(y - 1).toCharArray
  }

  def twoMatch(x: Integer, y: Integer): Unit = {
    val row = solutionArray(y).toCharArray
    val nextRow = solutionArray(y + 1).toCharArray
    val prevRow = solutionArray(y - 1).toCharArray
  }

  def threeMatch(x: Integer, y: Integer): Unit = {
    val row = solutionArray(y).toCharArray
    val nextRow = solutionArray(y + 1).toCharArray
    val prevRow = solutionArray(y - 1).toCharArray
  }

  def fourMatch(x: Integer, y: Integer): Unit = {
    val row = solutionArray(y).toCharArray
    val nextRow = solutionArray(y + 1).toCharArray
    val prevRow = solutionArray(y - 1).toCharArray
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
