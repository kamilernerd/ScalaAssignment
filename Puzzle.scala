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

    gameBoard.foreach((cell: Square) => {
      cell.getValue() match {
        case '0' => zeroMatch(cell)
        case '1' => definiteOneMatch(cell)
        case '2' => definiteTwoMatch(cell)
        case '3' => definiteThreeMatch(cell)
        case _ => cell
      }
    })

//    gameBoard.foreach((cell: Square) => {
//      cell.getValue() match {
//        case '2' => twoMatch(cell)
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
  def noLightInColumn(x: Int, start: Int = 0): Boolean = {
    var column = getColumn(x)

    var foundSomething = 0
    for (k <- start until column.length) {
      if (column(k).is('*')) {
        foundSomething += 1
      }

      if (column(k).is('X') || column(k).getValue().isDigit) {
        return true
      }

      if (foundSomething > 1) {
        return false
      }

      if (foundSomething == 0) {
        return true
      }
    }

    return true
  }

  // y is row
  def noLightInRow(y: Int): Boolean = {
    var row = getRow(y)

    var foundSomething = 0
    for (k <- 1 until row.length) {
      if (row(k).isNot('_')) {
        if (foundSomething == 0) {
          //          println("Found first illegal block in row:", row(k))
          foundSomething = 1
        }

        if (foundSomething == 1) {
          //          println("Found an illegal block at columnt:", row(k))
          return false
        }
      }
    }

    true
  }

  // x is index in row
  def noNumberInColumn(x: Int, start: Int = 0): Int = {
    var column = getColumn(x)
    for (k <- start until column.length) {
      if (column(k).getValue().isDigit) {
        return 0
      }
    }
    1
  }

  def setValue(x: Int,y: Int, value: Char): Boolean = {
    var square = getSquare(x, y)
    var newX = 0
    var newY = 0

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

    if(square.getValue() == value){
      return false;
    } else {
      gameBoard = gameBoard.filter(_ != square)
      square = square.setValue(value)
      gameBoard = gameBoard :+ square

      // Make sure to sort IT!!!!!
      gameBoard = gameBoard.sortBy((square: Square) => square.x).sortBy((square: Square) => square.y)

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

  def definiteOneMatch(cell: Square) = {
    // Check left, everything else is unavailable
    if (cell.x == 0 || getSquare(cell.x - 1, cell.y).is('_')) {
      if ((cell.y == 0 || getSquare(cell.x, cell.y - 1).isNot('_')) && // Above
        (cell.y == sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) && // Below
        (cell.x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) // Right
      )
      {
        setValue(cell.x - 1, cell.y, '*')
      }
    }

    // Check right, everything else is unavailable
    else if (getSquare(cell.x + 1, cell.y).is('_')) {
      if (cell.y == 0 || getSquare(cell.x, cell.y - 1).isNot('_') && // Above
        (cell.y == sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) && // Below
        (cell.x == 0 || getSquare(cell.x - 1, cell.y).isNot('_')) // Left
      ) {
        setValue(cell.x + 1, cell.y, '*')
      }
    }

    // Check above, everything else is unavailable
    else if (getSquare(cell.x, cell.y - 1).is('_')) {
      if (cell.y == 0 || getSquare(cell.x - 1, cell.y).isNot('_') && // Left
        (cell.x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) && // Right
        (cell.y == sizeY ||getSquare(cell.x, cell.y + 1).isNot('_')) // Below
      ) {
        setValue(cell.x, cell.y - 1, '*')
      }
    }

    // Check below, everything else is unavailable
    else if (getSquare(cell.x, cell.y + 1).is('_')) {
      if ((cell.x == 0 || getSquare(cell.x - 1, cell.y).isNot('_')) && // Left
        (cell.x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) && // Right
        (cell.y == 0 || getSquare(cell.x, cell.y - 1).isNot('_')) // Above
      ) {
        setValue(cell.x, cell.y + 1, '*')
      }
    }
  }
  

  def definiteTwoMatch(cell: Square) = {

    // top left
    if (cell.x == 0 && cell.y == 0) { //check first corner
      setValue(cell.x, cell.y + 1, '*')
      setValue(cell.x + 1, cell.y, '*')
    }

    // top right
    else if (cell.x == sizeX - 1 && cell.y == 0) {
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
    }

    // bottom left
    else if (cell.y == sizeY - 1 && cell.x == 0) {
      setValue(cell.x, cell.y - 1, '*')
      setValue(cell.x + 1, cell.y, '*')
    }

    // bottom right
    else if (cell.y == sizeY - 1 && cell.x == sizeX - 1) {
      setValue(cell.x, cell.y - 1, '*')
      setValue(cell.x - 1, cell.y, '*')
    }

    //    // x +- 1 placement - above&&below unavailable
    //    if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x + 1, cell.y).is('_')) {
    //      if(y == 0 || getSquare(cell.x, cell.y - 1).isNot('_') && //check above, y - 1
    //        (x == sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) // check below, y + 1
    //      ){
    //        setValue(cell.x - 1, cell.y, '*')
    //        setValue(cell.x + 1, cell.y, '*')
    //      }
    //    }
    //
    //    // x - 1 and above
    //    else if(getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x, cell.y - 1).is('_')){
    //      if((cell.x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_') && //right, x + 1
    //        (cell.y == 0 || getSquare(cell.x, cell.y - 1).isNot('_')) //above, y - 1
    //        )
    //      ) {
    //        setValue(cell.x - 1, cell.y, '*')
    //        setValue(cell.x, cell.y + 1, '*')
    //      }
    //    }
    //
    //    // x - 1, below
    //    else if(getSquare(cell.x - 1, cell.y).is('_') &&  getSquare(cell.x, cell.y + 1).is('_')){
    //      if(x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_') && //check x + 1
    //        (y == sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) //below, y + 1
    //      ) {
    //        setValue(cell.x - 1, cell.y, '*')
    //        setValue(cell.x, cell.y + 1, '*')
    //      }
    //    }
    //
    //    //below, x + 1
    //    else if(getSquare(cell.x + 1, cell.y).is('_') && getSquare(cell.x, cell.y - 1).is('_')){
    //      if(y == 0 || getSquare(cell.x - 1, cell.y).isNot('_') && //x - 1
    //        (y == sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) //below
    //      ) {
    //        setValue(cell.x + 1, cell.y, '*')
    //        setValue(cell.x, cell.y - 1, '*')
    //      }
    //    }
    //
    //    //below, x + 1
    //    else if(getSquare(cell.x + 1, cell.y).is('_') && getSquare(cell.x, cell.y + 1).is('_')){
    //      if((x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) && //x + 1
    //        (y == sizeX || getSquare(cell.x, cell.y + 1).isNot('_')) //below
    //      ){
    //        setValue(cell.x + 1, cell.y, '*')
    //        setValue(cell.x, cell.y + 1, '*')
    //      }
    //    }
    //
    //    //below, above
    //    else if(getSquare(cell.x, cell.y - 1).is('_') && getSquare(cell.x, cell.y + 1).is('_')){
    //      if((cell.y == 0 || getSquare(cell.x - 1, cell.y).isNot('_')) && //check above, y - 1
    //        (cell.y == sizeY || getSquare(cell.x + 1, cell.y).isNot('_')) //check below, y + 1
    //      ) {
    //        setValue(cell.x, cell.y - 1, '*')
    //        setValue(cell.x, cell.y + 1, '*')
    //      }
    //    }

  }

  def definiteThreeMatch(cell: Square) = {
    // Check left is unavailable
    if (cell.x - 1 <= 0) {
      println("Case left")
      setValue(cell.x + 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
      setValue(cell.x, cell.y - 1, '*')
    }

    else if (cell.x + 1 >= sizeX) { // Check right is unavailable
      println("Case right")
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
      setValue(cell.x, cell.y - 1, '*')
    }

    else if(cell.y - 1 <= 0) { // Check above is unavailable
      println("Case above")
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x + 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
    }

    else if(cell.y + 1 >= sizeY) { // Check below is unavailable
      println("Case below")
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x + 1, cell.y, '*')
      setValue(cell.x, cell.y - 1, '*')
    }
  }

  /**
   *
   * NORMAL MATCHES
   *
   */

  /**
   *  Left + right ---
   *  Left + above ---
   *  Left + below ---
   *  Right + above ---
   *  Right + below ---
   */
  def twoMatch(cell: Square): Unit = {
    // Left + right
    if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x + 1, cell.y).is('_')) {
      // Check if valid placement in left column
      if (noLightInColumn(cell.x - 1)) {
        if (noNumberInColumn(cell.x - 1) == 1) {
          setValue(cell.x - 1, cell.y, '*')
        } else if (noNumberInColumn(cell.x - 1) == 0) {
          if (getSquare(cell.x, cell.y - 1).is('_')) {
            setValue(cell.x, cell.y - 1, '*')
          } else if (getSquare(cell.x, cell.y + 1).is('_')) {
            setValue(cell.x, cell.y + 1, '*')
          }
        }
      }

      if (noLightInColumn(cell.x + 1)) {
        if (noNumberInColumn(cell.x + 1) == 0) {
          setValue(cell.x + 1, cell.y, '*')
        } else if (noNumberInColumn(cell.x + 1) == 1) {
          if (getSquare(cell.x, cell.y + 1).is('_')) {
            setValue(cell.x, cell.y + 1, '*')
          } else if (getSquare(cell.x, cell.y - 1).is('_')) {
            setValue(cell.x, cell.y - 1, '*')
          }
        }
      }
    }

    // Left + above
    else if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x, cell.y - 1).is('_')) {
      if (noLightInColumn(cell.x - 1)) {
        setValue(cell.x - 1, cell.y, '*')
      }

      if (noLightInRow(cell.y - 1)) {
        setValue(cell.x, cell.y - 1, '*')
      }
    }

    // Left + below
    else if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x, cell.y + 1).is('_')) {
      if (noLightInColumn(cell.x - 1)) {
        setValue(cell.x - 1, cell.y, '*')
      }

      if (noLightInRow(cell.y + 1)) {
        setValue(cell.x, cell.y + 1, '*')
      }
    }

    // Right + above
    else if (getSquare(cell.x + 1, cell.y).is('_') && getSquare(cell.x, cell.y - 1).is('_')) {
      if (noLightInColumn(cell.x + 1)) {
        setValue(cell.x + 1, cell.y, '*')
      }

      if (noLightInRow(cell.y - 1)) {
        setValue(cell.x, cell.y - 1, '*')
      }
    }

    // Right + below
    else if (getSquare(cell.x + 1, cell.y).is('*') && getSquare(cell.x, cell.y + 1).is('_')) {
      if (noLightInColumn(cell.x + 1)) {
        setValue(cell.x + 1, cell.y, '*')
      }

      if (noLightInRow(cell.y - 1)) {
        setValue(cell.x, cell.y - 1, '*')
      }
    }
  }

  def fourMatch(cell: Square) = {
    setValue(cell.x + 1, cell.y, '*')
    setValue(cell.x - 1, cell.y, '*')
    setValue(cell.x, cell.y + 1, '*')
    setValue(cell.x, cell.y - 1, '*')
  }
}
