import java.util

// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  var solutionArray = solution.split("\n");

  println(solutionArray.map(_.mkString(" ")).mkString("\n"));

  println("\n");

  var i = 0;
  for (i <- 0 until solutionArray.length) {
    walk(i, solutionArray(i).toCharArray);
  }

  println(solutionArray.map(_.mkString(" ")).mkString("\n"));

  def walk(yVal: Integer, row: Array[Char]): Unit = {
    var i = 0;

    while (i < row.length) {
      val x = i;
      val y = yVal;

      val next = y + 1
      val prev = y - 1

      val prevRow = if (prev >= 0) solutionArray(prev).toCharArray else solutionArray.head.toCharArray;
      val nextRow = if (next < solutionArray.size) solutionArray(next).toCharArray else solutionArray(y).toCharArray

      row(i).charValue() match {
        case '0' => zeroMatch(x, y, row, prevRow, nextRow)
        case '1' => oneMatch(x, y, row, prevRow, nextRow)
        case '2' => twoMatch(x, y, row, prevRow, nextRow)
        case '3' => threeMatch(x, y, row, prevRow, nextRow)
        case '4' => fourMatch(x, y, row, prevRow, nextRow)
        //case 'X' => fourMatch(x, y)
        //case '*' => fourMatch(x, y)
        //case '_' => fourMatch(x, y)
        case _  => row
      }
      i += 1;
    }
  }

  var hasFoundLight = 0;
  def hasLightInRow(pos: Integer, row: Array[Char]): Boolean = {
    if (pos == row.length) {
      println("Length reached", row.length);
      return false;
    }

    if (row(pos) == '*') {
      if (hasFoundLight == 1) {
        hasFoundLight = 0;
        println("Found light ", row, "pos ", pos);
        return true;
      } else {
        hasFoundLight = 1;
      }
    }
    hasLightInRow(pos + 1, row);
  }

  def zeroMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {
    // Case 1 - Check left
    if (row(x - 1).charValue() == '_') {
      val rown = row.updated(x - 1, '~');
      solutionArray = solutionArray.updated(y, String.valueOf(rown));
    }

    // Case 2 - Check right
    if (row(x + 1).charValue() == '_') {
      val rown = row.updated(x + 1, '~');
      solutionArray = solutionArray.updated(y, String.valueOf(rown));
    }

    // Case 4 - Check above
    if (prevRow(x).charValue() == '_') {
      val rown = prevRow.updated(x, '~');
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown));
    }

    // Case 6 - Check below
    if (nextRow(x).charValue() == '_') {
      val rown = nextRow.updated(x, '~');
      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown));
    }
  }

  def oneMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {

  }

  def twoMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {
    // Not a star to the left
    if (row(x - 1) != '*') {
      // Not X or 0 or ~
      if (row(x - 1) != 'X' && row(x - 1) != '0' && row(x - 1) != '~' && row(x) != '1') {
        // See what's above, below, and one more to the left
        // Then see what's above - 1 and above + 1
        // Then see what's below - 1 and below + 1

        // Check above
        if (prevRow(x) != '0' && prevRow(x) != '~' && prevRow(x) != '*' && prevRow(x) != 'X' && prevRow(x) != '1') {

          // Check above - 1
          if (prevRow(x - 1) != '0' && prevRow(x - 1) != '~' && prevRow(x - 1) != '*' && prevRow(x - 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x - 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }

          // Check above + 1
          if (prevRow(x + 1) != '0' && prevRow(x + 1) != '~' && prevRow(x + 1) != '*' && prevRow(x + 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x + 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }
        }

        // Check below
        if (nextRow(x) != '0' && nextRow(x) != '~' && nextRow(x) != '*' && nextRow(x) != 'X' && nextRow(x) != '1') {

          // Check below - 1
          if (nextRow(x - 1) != '0' && nextRow(x - 1) != '~' && nextRow(x - 1) != '*' && nextRow(x - 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x - 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }

          // Check below + 1
          if (nextRow(x + 1) != '0' && nextRow(x + 1) != '~' && nextRow(x + 1) != '*' && nextRow(x + 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x + 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }
        }
      }
    }
    // Not a star to the right
    else if (row(x + 1) != '*') {
      // Not X or 0 or ~
      if (row(x + 1) != 'X' && row(x + 1) != '0' && row(x + 1) != '~' && row(x + 1) != '1') {
        // See what's above, below, and one more to the left
        // Then see what's above - 1 and above + 1
        // Then see what's below - 1 and below + 1

        // Check above
        if (prevRow(x) != '0' && prevRow(x) != '~' && prevRow(x) != '*' && prevRow(x) != 'X' && prevRow(x) != '1') {

          // Check above - 1
          if (prevRow(x - 1) != '0' && prevRow(x - 1) != '~' && prevRow(x - 1) != '*' && prevRow(x - 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x - 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }

          // Check above + 1
          if (prevRow(x + 1) != '0' && prevRow(x + 1) != '~' && prevRow(x + 1) != '*' && prevRow(x + 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x + 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }
        }

        // Check below
        if (nextRow(x) != '0' && nextRow(x) != '~' && nextRow(x) != '*' && nextRow(x) != 'X' && nextRow(x) != '1') {

          // Check below - 1
          if (nextRow(x - 1) != '0' && nextRow(x - 1) != '~' && nextRow(x - 1) != '*' && nextRow(x - 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x - 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }

          // Check below + 1
          if (nextRow(x + 1) != '0' && nextRow(x + 1) != '~' && nextRow(x + 1) != '*' && nextRow(x + 1) != '1' && !hasLightInRow(0, solutionArray(1).toCharArray)) {
            val rown = row.updated(x + 1, '*');
            solutionArray = solutionArray.updated(y, String.valueOf(rown));
          }
        }
      }
    }
  }

  def threeMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {}

  def fourMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {}

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
