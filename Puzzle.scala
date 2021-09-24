// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  val solutionArray = solution.split("\n")

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
    for (y <- 0 until solutionArray.length) {
      var row = solutionArray(y).toCharArray

      var x = 0
      for (x <- 0 until row.length) {
        val cellValueX = row(x)
        val square = new Square(x, y, cellValueX)
        gameBoard = gameBoard :+ square
      }
    }

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
    for (k <- 0 until sizeY) {
      for (j <- 0 until sizeX) {
        val square = getSquare(j, k)
        tmp += square.getValue()
      }
    }
    return tmp.grouped(sizeX).mkString("\n")
  }

  def getSquare(x: Int,y: Int): Square = {
    var newX = x
    var newY = y

    if (!(x >= 0)) {
      newX = 0
    }

    if (!(x <= sizeX)) {
      newX = sizeX
    }

    if (!(y >= 0)) {
      newY = 0
    }

    if (!(y <= sizeY)) {
      newY = sizeY
    }

    return gameBoard.filter(_.x == newX).filter(_.y == newY)(0)
  }

  def getColumn(y: Int): List[Square] = {
    return gameBoard.filter((col: Square) => col.x == y)
  }

  def getRow(x: Int): List[Square] = {
    return gameBoard.filter((row: Square) => row.y == x)
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
    if (getSquare(cell.x - 1, cell.y).getValue() == '_') { // Check left
      if(getSquare(cell.x, cell.y - 1).getValue() != '_' && getSquare(cell.x, cell.y + 1).getValue() != '_') {
        setValue(cell.x - 1, cell.y, '*')
      }
    }
    else if (getSquare(cell.x + 1, cell.y).getValue() == '_') { // Check right
      if(getSquare(cell.x, cell.y - 1).getValue() != '_' && getSquare(cell.x, cell.y + 1).getValue() != '_') {
        setValue(cell.x + 1, cell.y, '*')
      }
    } else if (getSquare(cell.x, cell.y + 1).getValue() != '_') { //check prev
      if(getSquare(cell.x + 1, cell.y).getValue() == '_' && getSquare(cell.x - 1, cell.y).getValue() == '_'){
        setValue(cell.x, cell.y - 1, '*')
      }
    }
    else if (getSquare(cell.x, cell.y - 1).getValue() != '_') { //check next
      if(getSquare(cell.x + 1, cell.y).getValue() == '_' && getSquare(cell.x - 1, cell.y).getValue() == '_'){
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
