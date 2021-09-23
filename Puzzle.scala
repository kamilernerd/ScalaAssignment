import java.util

// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
//  var solutionArray = solution.split("\n");

//  var solutionArray = Array[String](
//"____1___",
//     "____XX__",
//     "__2__1_1",
//     "_21_____",
//     "__1_____",
//     "___2____",
//     "X___2_2_",
//     "__02___X",
//     "_____1__"
//  )

  var solutionArray = Array[String](
    "2___1__2",
    "____XX__",
    "__2__1_1",
    "_21_____",
    "__1_____",
    "___2____",
    "X___2_2_",
    "__02____",
    "2____1_2"
  )

  def walk(): Unit = {
    var i = 0;
    for (i <- 0 until solutionArray.length) {

      var row = solutionArray(i).toCharArray;

      var k = 0;
      for (k <- 0 until row.length) {

        val next = i + 1
        val prev = i - 1

        row = solutionArray(i).toCharArray;
        val prevRow = if (prev >= 0) solutionArray(prev).toCharArray else solutionArray.head.toCharArray;
        val nextRow = if (next < solutionArray.length) solutionArray(next).toCharArray else solutionArray(i).toCharArray

        row(k)  match { //definite matching
          case '0' => zeroMatch(k, i, row, prevRow, nextRow)
          case '1' => DefiniteOneMatch(k, i, row, prevRow, nextRow)
          case '2' => DefiniteTwoMatch(k, i, row, prevRow, nextRow)
          case '3' => DefiniteThreeMatch(k, i, row, prevRow, nextRow)
          case '4' => fourMatch(k, i, row, prevRow, nextRow)
//          case 'X' => row
//          case '*' => row
//          case '~' => row
//          case '_' => row
          case _ =>
        }
      }

      //    var j = 0;
      //    while(j < 5){
      //      k = 0
      //      for (k <- 0 until row.length - 1) {
      //
      //        val prevRow = if (prev >= 0) solutionArray(prev).toCharArray else solutionArray.head.toCharArray;
      //        val nextRow = if (next < solutionArray.length) solutionArray(next).toCharArray else solutionArray(i).toCharArray
      //
      //        row(k).charValue() match { //definite matching
      //          case '1' => DefiniteOneMatch(k, i, row, prevRow, nextRow)
      //          case '2' => DefiniteTwoMatch(k, i, row, prevRow, nextRow)
      //          case '3' => DefiniteThreeMatch(k, i, row, prevRow, nextRow)
      //          case _ =>
      //        }
      //      }
      //      j += 1
      //    }

    }
  }

  var hasFoundLight = 0;
  def hasLightInRow(pos: Integer, row: Array[Char]): Boolean = {
    if (pos == row.length) {
      println("Length reached", row.length)
      return false
    }

    // Check if there is something between light, ex. * _ _ _ X _ * _
    if (hasFoundLight == 1 && row(pos) != '*' || hasFoundLight == 1 && row(pos) != '_') {
      hasFoundLight = 0;
      println("Found second light but something is blocking the way, resetting!", row, "pos ", pos)
      return false;
    }

    if (hasFoundLight == 1 && row(pos) != '*') {
      hasFoundLight = 0
      println("Found second light ", row, "pos ", pos)
      return true
    }

    if (row(pos) == '*' && hasFoundLight == 0) {
      hasFoundLight = 1
      return false;
    }

    hasLightInRow(pos + 1, row)
  }

  def inBoundsX(x: Integer, row: Array[Char]): Boolean = {
    if (x <= row.length && x >= 0) {
      return true;
    }
    return false;
  }

  def inBoundsY(y: Integer): Boolean = {
    if (y <= sizeY && y >= 0) {
      return true;
    }
    return false;
  }

  def lightUpRow(x:Integer, y: Integer, row: Array[Char]): Unit = {
    var xPos = x;
    while (xPos <= row.length) {
      if (row(xPos) == '_') {
        var rown = row.updated(xPos, 'k')
        solutionArray = solutionArray.updated(y, String.valueOf(rown))
      } else {
        return;
      }
      xPos += 1;
    }
  }

  def zeroMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {
    // Case 1 - Check left
    if (row(x - 1).charValue() == '_') {
      val rown = row.updated(x - 1, '~')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
    }

    // Case 2 - Check right
    if (row(x + 1).charValue() == '_') {
      val rown = row.updated(x + 1, '~')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
    }

    // Case 4 - Check above
    if (prevRow(x).charValue() == '_') {
      val rown = prevRow.updated(x, '~')
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
    }

    // Case 6 - Check below
    if (nextRow(x).charValue() == '_') {
      val rown = nextRow.updated(x, '~')
      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
    }
  }

  def DefiniteOneMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]): Unit = {

    if (x >= 0 && row(x - 1) == '_') { // Check left
      if(prevRow(x) != '_' && nextRow(x) != '_') {
        var rown = row.updated(x - 1, '*');
        solutionArray = solutionArray.updated(y, String.valueOf(rown));
      }
    }
    else if (x <= row.length - 1 && row(x + 1) == '_') { // Check right
      if(prevRow(x) != '_' && nextRow(x) != '_') {
        var rown = row.updated(x + 1, '*');
        solutionArray = solutionArray.updated(y, String.valueOf(rown));
      }
    } else if (nextRow(x) != '_') { //check prev
      if(row(x+1) != '_' && row(x-1) != '_'){
        var rown = prevRow.updated(x, '*')
        solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
      }
    }
    else if (prevRow(x) != '_') { //check next
      if(row(x+1) != '_' && row(x-1) != '_'){
        var rown = nextRow.updated(x, '*')
        solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
      }
    }
  }

  def DefiniteTwoMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {
    if(x == 0 && y == 0){ //check first corner
      var rown = row.updated(x + 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = nextRow.updated(x, '*')
      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
    }

    //check 2nd corner
    if(x == row.length - 1 && y == 0){
      var rown = nextRow.updated(x, '*')
      solutionArray = solutionArray.updated(y+1, String.valueOf(rown))
      rown = row.updated(x-1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
    }

    //3rd corner
    if(y == sizeY - 1 && x == 0){
      var rown = prevRow.updated(x, '*')
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
      rown = row.updated(x+1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
    }

    //4th corner
    if(y == sizeY - 1 && x == row.length - 1){
      var rown = prevRow.updated(x, '*')
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
      rown = row.updated(x - 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
    }

//    //x+1 && x-1 are unavailable, check for light bulbs
//    if (row(x+1) != '_' && row(x-1) != '_' && row(x+1) != '*' && row(x-1) != '*'){
//      var rown = prevRow.updated(x, '*')
//      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
//      rown = nextRow.updated(x, '*')
//      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
//    }
//
//    //prev and nextRow are unavailable
//    else if(prevRow(x) != '_' &&  nextRow(x) != '_' && prevRow(x) != '*' &&  nextRow(x) != '*'){
//      var rown = row.updated(x + 1, '*')
//      solutionArray = solutionArray.updated(y, String.valueOf(rown))
//      rown = row.updated(x - 1, '*')
//      solutionArray = solutionArray.updated(y, String.valueOf(rown))
//    }
//
//    //nextrow && x-1
//    else if(nextRow(x) != '_' || nextRow(x) != '*' || !nextRow(x).isDigit && row(x-1) != '_' || row(x-1) != '*' || !row(x-1).isDigit){
//      var rown = row.updated(x - 1, '*')
//      solutionArray = solutionArray.updated(y, String.valueOf(rown))
//      rown = prevRow.updated(x, '*')
//      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
//    }
//
//    //nextrow && x+1
//    else if(prevRow(x) != '_' && row(x + 1) != '_'){
//      var rown = row.updated(x + 1, '*')
//      solutionArray = solutionArray.updated(x + 1, String.valueOf(rown))
//      rown = nextRow.updated(x, '*')
//      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
//    }
//    //prev && x-1
//    else if(prevRow(x) != '_' && row(x-1) !=  '_'){
//      var rown = nextRow.updated(x, '*') //y???
//      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
//      rown = row.updated(x+1, '*')
//      solutionArray = solutionArray.updated(y, String.valueOf(rown))
//    }
//    //prev && x+1
//    else if(prevRow(x) != '_' && row(x+1) != '_'){
//      var rown = nextRow.updated(x, '*') //y???
//      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
//      rown = row.updated(x + 1, '*')
//      solutionArray = solutionArray.updated(y, String.valueOf(rown))
//    }
    // more cases? Doubt***
  }


  def DefiniteThreeMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {

    if (row(x - 1) != '_' || !inBoundsX(x - 1, row)){ //check if left unavailable
      var rown = row.updated(x + 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = nextRow.updated(x, '*')
      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
      rown = prevRow.updated(x, '*')
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
    }

    else if(row(x + 1) != '_' || !inBoundsX(x + 1, row)){ //check if right is unavailable
      var rown = row.updated(x - 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = nextRow.updated(x, '*')
      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
      rown = prevRow.updated(x, '*')
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
    }

    else if(prevRow(x) != '_' || !inBoundsY(y)){ //check above
      var rown = row.updated(x - 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = row.updated(x + 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = nextRow.updated(x, '*')
      solutionArray = solutionArray.updated(y + 1, String.valueOf(rown))
    }

    else if(nextRow(x) != '_' || !inBoundsY(y)){ //check below
      var rown = row.updated(x - 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = row.updated(x + 1, '*')
      solutionArray = solutionArray.updated(y, String.valueOf(rown))
      rown = prevRow.updated(x, '*')
      solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
    }
    //no more cases? imagine index out of bounds lmao. Check prevRow/nextRow or x-1/x+1 if it is

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
          if (prevRow(x - 1) != '0' && prevRow(x - 1) != '~' && prevRow(x - 1) != '*' && prevRow(x - 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x - 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }

          // Check above + 1
          if (prevRow(x + 1) != '0' && prevRow(x + 1) != '~' && prevRow(x + 1) != '*' && prevRow(x + 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x + 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }
        }

        // Check below
        if (nextRow(x) != '0' && nextRow(x) != '~' && nextRow(x) != '*' && nextRow(x) != 'X' && nextRow(x) != '1') {

          // Check below - 1
          if (nextRow(x - 1) != '0' && nextRow(x - 1) != '~' && nextRow(x - 1) != '*' && nextRow(x - 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x - 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }

          // Check below + 1
          if (nextRow(x + 1) != '0' && nextRow(x + 1) != '~' && nextRow(x + 1) != '*' && nextRow(x + 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x + 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
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
          if (prevRow(x - 1) != '0' && prevRow(x - 1) != '~' && prevRow(x - 1) != '*' && prevRow(x - 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x - 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }

          // Check above + 1
          if (prevRow(x + 1) != '0' && prevRow(x + 1) != '~' && prevRow(x + 1) != '*' && prevRow(x + 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x + 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }
        }

        // Check below
        if (nextRow(x) != '0' && nextRow(x) != '~' && nextRow(x) != '*' && nextRow(x) != 'X' && nextRow(x) != '1') {

          // Check below - 1
          if (nextRow(x - 1) != '0' && nextRow(x - 1) != '~' && nextRow(x - 1) != '*' && nextRow(x - 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x - 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }

          // Check below + 1
          if (nextRow(x + 1) != '0' && nextRow(x + 1) != '~' && nextRow(x + 1) != '*' && nextRow(x + 1) != '1' && !hasLightInRow(0, row)) {
            val rown = row.updated(x + 1, '*')
            solutionArray = solutionArray.updated(y, String.valueOf(rown))
          }
        }
      }
    }
  }

  def threeMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {

  }

  def fourMatch(x: Integer, y: Integer, row: Array[Char], prevRow: Array[Char], nextRow: Array[Char]) = {
    var rown = row.updated(x + 1, '*');
    solutionArray = solutionArray.updated(y, String.valueOf(rown));

    rown = row.updated(x - 1, '*');
    solutionArray = solutionArray.updated(y, String.valueOf(rown));

    rown = row.updated(nextRow(x), '*');
    solutionArray = solutionArray.updated(y + 1, String.valueOf(rown));

    rown = row.updated(prevRow(x), '*');
    solutionArray = solutionArray.updated(y - 1, String.valueOf(rown))
  }

  walk();

  println(solutionArray.map(_.mkString(" ")).mkString("\n"));

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }
}
