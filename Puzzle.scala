// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  val solutionArray = solution.split("\n").filter(_.nonEmpty)
  var gameBoard = List[Square]();

  //     "____1___",
  //     "____XX__",
  //     "__2__1_1",
  //     "_21_____",
  //     "__1_____",
  //     "___2____",
  //     "X___2_2_",
  //     "__02___X",
  //     "_____1__"

  def walk(): Puzzle = {
    for (yVal <- 0 until solutionArray.length) {
      var row = solutionArray(yVal).toCharArray

      for (xVal <- 0 until row.length) {
        val cellValueX = row(xVal)
        val square = new Square(xVal, yVal, cellValueX)
        gameBoard = gameBoard :+ square
      }
    }

    println(gameBoard)

    gameBoard.foreach((cell: Square) => {
      cell.getValue() match {
        case '0' => zeroMatch(cell)
        case '1' => definiteOneMatch(cell)
        case _ => cell
      }
    })

//    gameBoard.foreach((cell: Square) => {
//      cell.getValue() match { //brute force
//        case '1' => oneMatch(cell)
//        case _ => cell
//      }
//    })

    println(prettyPrint())

    return new Puzzle(x, y, solution)
  }

  def prettyPrint(): String = {
    var tmp = ""
    for (k <- 0 to sizeY - 1) {
      for (j <- 0 to sizeX - 1) {
        val square = getSquare(j, k)
        tmp += square.getValue()
      }
    }
    return tmp.grouped(sizeX).mkString("\n")
  }

  def getSquare(x: Int,y: Int): Square = {
    var newX = x
    var newY = y

    if (x < 0) {
      newX = 0
    }

    if (x > sizeX) {
      newX = sizeX
    }

    if (y < 0) {
      newY = 0
    }

    if (y > sizeY) {
      newY = sizeY
    }

    return gameBoard.filter(_.x == newX).filter(_.y == newY).head
  }

  /**
   * x param is actually index in row
   * @param y
   * @return
   */
  def getColumn(y: Int): List[Square] = {
    return gameBoard.filter((col: Square) => col.x == y)
  }

  /**
   * y param is actually position from top to bottom
   * @param x
   * @return
   */
  def getRow(x: Int): List[Square] = {
    return gameBoard.filter((row: Square) => row.y == x)
  }

  // x is index in row
  def checkForLightInColumn(x: Int): Boolean = {
    var column = getColumn(x)

    var hasFoundLight = 0

    var k = 0
    for (k <- 0 until column.length) {
      if (column(k).is('*') && hasFoundLight == 0) {
        hasFoundLight += 1
        //println("found light in row")
      } else if ((column(k).is('*') && hasFoundLight == 1) || (column(k).is('X') || column(k).getValue().isDigit)) {
        //println("found light in row and another one or something else")
        return true
      } else {
        return false
      }
    }
    return false
  }

  def setValue(x: Int,y: Int, value: Char): Boolean = {
    var square = getSquare(x, y)
    if(square.getValue() == value){
      return false;
    } else {
      gameBoard = gameBoard.filter(_ != square)
      square = square.setValue(value)
      gameBoard = gameBoard :+ square
      return true;
    }
  }

  override def toString: String = {
    s"${sizeX}x${sizeY} -->\n${solution}"
  }


  /**
   *
   * DEFINITE MATCHES
   *
   */
  def definiteOneMatch(cell: Square) = {
    if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x + 1, cell.y).isNot('_')) { // Check left
      if (getSquare(cell.x, cell.y - 1).getValue() != '_' && getSquare(cell.x, cell.y + 1).getValue() != '_') {
        setValue(cell.x - 1, cell.y, '*')
      }
    }
    else if (getSquare(cell.x + 1, cell.y).getValue() == '_' && !checkForLightInColumn(cell.x + 1)) { // Check right
      if(getSquare(cell.x, cell.y - 1).getValue() != '_' && getSquare(cell.x, cell.y + 1).getValue() != '_') {
        setValue(cell.x + 1, cell.y, '*')
      }
    } else if (getSquare(cell.x, cell.y + 1).getValue() != '_' && !checkForLightInColumn(cell.x)) { //check prev
      if(getSquare(cell.x + 1, cell.y).getValue() == '_' && getSquare(cell.x - 1, cell.y).getValue() == '_') {
        setValue(cell.x, cell.y - 1, '*')
      }
    }
    else if (getSquare(cell.x, cell.y - 1).getValue() != '_' && !checkForLightInColumn(cell.x)) { //check next
      if(getSquare(cell.x + 1, cell.y).getValue() == '_' && getSquare(cell.x - 1, cell.y).getValue() == '_') {
        setValue(cell.x, cell.y + 1, '*')
      }
    }
  }

  /**
   *
   * NORMAL MATCHES
   *
   */

  def zeroMatch(cell: Square) = {
    // Case 1 - Check left
    if (getSquare(cell.x - 1, cell.y).getValue() == '_') {
      setValue(cell.x - 1, cell.y, '~')
    }

    // Case 2 - Check right
    if (getSquare(cell.x + 1, cell.y).getValue() == '_') {
      setValue(cell.x + 1, cell.y, '~')
    }

    // Case 4 - Check above
    if (getSquare(cell.x, cell.y - 1).getValue() == '_') {
      setValue(cell.x, cell.y - 1, '~')
    }

    // Case 6 - Check below
    if (getSquare(cell.x, cell.y + 1).getValue() == '_') {
      setValue(cell.x, cell.y + 1, '~')
    }
  }

  def fourMatch(cell: Square) = {
    setValue(cell.x + 1, cell.y, '*')
    setValue(cell.x - 1, cell.y, '*')
    setValue(cell.x, cell.y + 1, '*')
    setValue(cell.x, cell.y - 1, '*')
  }
}
