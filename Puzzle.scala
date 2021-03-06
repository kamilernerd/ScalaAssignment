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

    def defMatches() = {
      gameBoard.foreach((cell: Square) => {
        cell.getValue() match {
          case '0' => zeroMatch(cell)
          //case '1' => definiteOneMatch(cell)
          case '2' => definiteTwoMatch(cell)
          case '3' => definiteThreeMatch(cell)
          case '4' => fourMatch(cell)
          case _ => isolatedMatch(cell)
        }
      })
    }
    defMatches()


    gameBoard.foreach((cell: Square) => {
      cell.getValue() match {
        //case '2' => twoMatch(cell)
        case _ => cell
      }
    })

    val solvedBoardString = prettyPrint()

    println(solvedBoardString + "\n")
    //  lmao
    //    val cleanedUpString = replaceChars(solvedBoardString)
    //    if (!cleanedUpString.equals("_X__1*____*__XX*_*2_*1_1*21*______1__*____*2__*_X__*2_2**_02*__X_*___1*_")) {
    //      return walk()
    //    }
    return new Puzzle(x, y, prettyPrint())
  }

  //  def replaceChars(s: String) = s.map(c => if(c == '~') '_' else c)
  //
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

  def getSquare(x: Int, y: Int): Square = {
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
   *
   * @param y
   * @return
   */
  def getColumn(y: Int): List[Square] = {
    return gameBoard.filter((col: Square) => col.x == y)
  }

  /**
   * y param is actually position from top to bottom
   *
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

  def isLightConnectedToOne(cell: Square): Boolean = {
    if (cell.x > 0 && getSquare(cell.x - 1, cell.y).is('*')) { // check left
      return true
    }

    if (cell.x + 1 < sizeX && getSquare(cell.x + 1, cell.y).is('*')) { // check right
      return true
    }

    if (cell.y > 0 && getSquare(cell.x, cell.y - 1).is('*')) {
      return true
    }

    if (cell.y + 1 < sizeY && getSquare(cell.x, cell.y + 1).is('*')) {
      return true
    }

    return false
  }

  def setValue(x: Int, y: Int, value: Char): Boolean = {
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

    if (square.getValue() == value) {
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

  /*
  Definite function that loops over rows with ~
   */
  def cellChangeToTilde(cell: Square, xCord: Int, yCord: Int): Unit = { //SEND SQUARE WHERE LIGHTBULB IS PLACED && WHICH CELL HAS LIGHTBULB

    val xVal = xCord //xVal is to keep a copy of original spot SHOULD NOT BE MUTABLE
    val yVal = yCord //yVal is to keep a copy of original spot SHOULD NOT BE MUTABLE

    var xVal1 = xCord + 1 //to loop over right side
    var xVal2 = xCord - 1 //to loop over left side

    var yVal1 = yCord + 1 //to loop below
    var yVal2 = yCord - 1 //to loop above

    while (xVal1 < sizeX && (getSquare(xVal1, yVal).is('_') || getSquare(xVal1, yVal).is('~'))) { //loop over right side till either something a wall is hit or edge
      setValue(xVal1, yVal, '~')
      xVal1 += 1
    }

    while (xVal2 >= 0 && (getSquare(xVal2, yVal).is('_') || getSquare(xVal2, yVal).is('~'))) { //loop over left side till either something a wall is hit or edge
      setValue(xVal2, yVal, '~')
      xVal2 -= 1
    }

    while (yVal1 < sizeY && (getSquare(xVal, yVal1).is('_') || getSquare(xVal, yVal1).is('~'))) { //loop over below, till either something a wall is hit or edge
      setValue(xVal, yVal1, '~')
      yVal1 += 1
    }

    while (yVal2 >= 0 && (getSquare(xVal, yVal2).is('_') || getSquare(xVal, yVal2).is('~'))) { // loop over above, till either something a wall is hit or edge
      setValue(xVal, yVal2, '~')
      yVal2 -= 1
    }
  }

  /**
   *
   * DEFINITE MATCHES
   *
   */

  def isolatedMatch(cell: Square) = {
    //if a cell is isolated and has to light itself up
    val xCord = cell.x
    val yCord = cell.y

//    if ((xCord - 1 >= 0 || getSquare(cell.x - 1, cell.y).isNot('_'))          //left
//      && (xCord + 1 < sizeX || getSquare(cell.x + 1, cell.y).isNot('_'))      //right
//      && (yCord - 1 >= 0 || getSquare(cell.x, cell.y - 1).isNot('_'))         //above
//      && (yCord + 1 < sizeY || getSquare(cell.x, cell.y + 1).isNot('_'))){    //below
//      setValue(cell.x, cell.y, '*')
//    }
  }


  def zeroMatch(cell: Square) = {
    val xCord = cell.x
    val yCord = cell.y
    // Case 1 - Check left
    if (xCord - 1 >= 0 && getSquare(cell.x - 1, cell.y).is('_')) {
      setValue(cell.x - 1, cell.y, '~')
    }

    // Case 2 - Check right
    if (cell.x + 1 < sizeX && getSquare(cell.x + 1, cell.y).is('_')) {
      setValue(cell.x + 1, cell.y, '~')
    }

    // Case 4 - Check above
    if (yCord - 1 >= 0 && getSquare(cell.x, cell.y - 1).is('_')) {
      setValue(cell.x, cell.y - 1, '~')
    }

    // Case 6 - Check below
    if (yCord + 1 < sizeY && getSquare(cell.x, cell.y + 1).is('_')) {
      setValue(cell.x, cell.y + 1, '~')
    }
  }


  def definiteOneMatch(cell: Square) = {
    // Check left, everything else is unavailable
    if (cell.x == 0 || getSquare(cell.x - 1, cell.y).is('_')) {
      if ((cell.y == 0 || getSquare(cell.x, cell.y - 1).isNot('_')) && // Above
        (cell.y + 1 < sizeY && getSquare(cell.x, cell.y + 1).isNot('_')) && // Below
        (cell.x > sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) // Right
      ) {
        setValue(cell.x - 1, cell.y, '*')
        cellChangeToTilde(cell, cell.x - 1, cell.y)
      }
    }

    // Check right, everything else is unavailable
    else if (getSquare(cell.x + 1, cell.y).is('_')) {
      if ((cell.x - 1 >= 0 && getSquare(cell.x - 1, cell.y).isNot('_')) &&         //check left
        (cell.y + 1 < sizeY && getSquare(cell.x, cell.y + 1).isNot('_')) &&  //check below
          (cell.y - 1 >= 0 && getSquare(cell.x, cell.y - 1).isNot('_'))      //check above
      )
          {
        setValue(cell.x + 1, cell.y, '*')
        cellChangeToTilde(cell, cell.x + 1, cell.y)
      }
    }

    // Check above, everything else is unavailable
    else if (getSquare(cell.x, cell.y - 1).is('_')) {
      if (cell.x - 1 == 0 || getSquare(cell.x - 1, cell.y).isNot('_') && // Left
        (cell.x + 1 < sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) && // Right
        (cell.y + 1 < sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) // Below
      ) {
        setValue(cell.x, cell.y - 1, '*')
        cellChangeToTilde(cell, cell.x, cell.y - 1)
      }
    }

    // Check below, everything else is unavailable
    else if (getSquare(cell.x, cell.y + 1).is('_')) {
      if ((cell.x == 0 || getSquare(cell.x - 1, cell.y).isNot('_')) && // Left
        (cell.x == sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) && // Right
        (cell.y == 0 || getSquare(cell.x, cell.y - 1).isNot('_')) // Above
      ) {
        setValue(cell.x, cell.y + 1, '*')
        cellChangeToTilde(cell, cell.x, cell.y + 1)
      }
    }
  }

  def definiteTwoMatch(cell: Square) = {

    // top left
    if (cell.x == 0 && cell.y == 0) { //check first corner
      setValue(cell.x, cell.y + 1, '*')
      setValue(cell.x + 1, cell.y, '*')
      cellChangeToTilde(cell, cell.x, cell.y + 1)
      cellChangeToTilde(cell, cell.x + 1, cell.y)
    }

    // top right
    else if (cell.x == sizeX - 1 && cell.y == 0) {
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
      cellChangeToTilde(cell, cell.x - 1, cell.y)
      cellChangeToTilde(cell, cell.x, cell.y + 1)
    }

    // bottom left
    else if (cell.y == sizeY - 1 && cell.x == 0) {
      setValue(cell.x, cell.y - 1, '*')
      setValue(cell.x + 1, cell.y, '*')
      cellChangeToTilde(cell, cell.x, cell.y - 1)
      cellChangeToTilde(cell, cell.x + 1, cell.y)
    }

    // bottom right
    else if (cell.y == sizeY - 1 && cell.x == sizeX - 1) {
      setValue(cell.x, cell.y - 1, '*')
      setValue(cell.x - 1, cell.y, '*')
      cellChangeToTilde(cell, cell.x, cell.y - 1)
      cellChangeToTilde(cell, cell.x - 1, cell.y)
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
    if (cell.x - 1 <= 0 || getSquare(cell.x - 1, cell.y).isNot('_')) {
      println("Case left")
      setValue(cell.x + 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
      setValue(cell.x, cell.y - 1, '*')

      cellChangeToTilde(cell, cell.x + 1, cell.y) //send every light bulb to change
      cellChangeToTilde(cell, cell.x, cell.y - 1)
      cellChangeToTilde(cell, cell.x, cell.y + 1)
    }

    else if (cell.x + 1 >= sizeX || getSquare(cell.x + 1, cell.y).isNot('_')) { // Check right is unavailable
      println("Case right")
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')
      setValue(cell.x, cell.y - 1, '*')

      cellChangeToTilde(cell, cell.x - 1, cell.y) //send every light bulb to change
      cellChangeToTilde(cell, cell.x, cell.y - 1)
      cellChangeToTilde(cell, cell.x, cell.y + 1)
    }

    else if (cell.y - 1 <= 0 || getSquare(cell.x, cell.y - 1).isNot('_')) { // Check above is unavailable
      println("Case above")
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x + 1, cell.y, '*')
      setValue(cell.x, cell.y + 1, '*')

      cellChangeToTilde(cell, cell.x - 1, cell.y) //send every light bulb to change
      cellChangeToTilde(cell, cell.x + 1, cell.y)
      cellChangeToTilde(cell, cell.x, cell.y + 1)
    }

    else if (cell.y + 1 >= sizeY || getSquare(cell.x, cell.y + 1).isNot('_')) { // Check below is unavailable
      println("Case below")
      setValue(cell.x - 1, cell.y, '*')
      setValue(cell.x + 1, cell.y, '*')
      setValue(cell.x, cell.y - 1, '*')

      cellChangeToTilde(cell, cell.x - 1, cell.y) //send every light bulb to change
      cellChangeToTilde(cell, cell.x + 1, cell.y)
      cellChangeToTilde(cell, cell.x, cell.y - 1)
    }
  }

  /**
   *
   * NORMAL MATCHES
   *
   */

  /**
   * Left + right ---
   * Left + above ---
   * Left + below ---
   * Right + above ---
   * Right + below ---
   */
  def twoMatch(cell: Square): Unit = {
    // Has only one light connected
    // Left
    if (getSquare(cell.x - 1, cell.y).is('*') &&
      (getSquare(cell.x + 1, cell.y).isNot('*') || getSquare(cell.x, cell.y - 1).isNot('*') || getSquare(cell.x, cell.y + 1).isNot('*'))
    ) {
      // Check if can be placed to the right
      if (getSquare(cell.x + 1, cell.y).is('_') && noNumberInColumn(cell.x + 1) == 1) {
        setValue(cell.x + 1, cell.y, '*')
      }
      // Check if can be placed above
      else if (getSquare(cell.x, cell.y - 1).is('*') && noNumberInColumn(cell.y - 1) == 1) {
        setValue(cell.x, cell.y - 1, '*')
      }
      // Check if can be placec below
      else if (getSquare(cell.x, cell.y + 1).is('*') && noLightInRow(cell.y + 1) == 1) {
        setValue(cell.x, cell.y + 1, '*')
      }
    }
    // Right
    else if (getSquare(cell.x + 1, cell.y).is('*') &&
      (getSquare(cell.x - 1, cell.y).isNot('*') || getSquare(cell.x, cell.y - 1).isNot('*') || getSquare(cell.x, cell.y + 1).isNot('*'))
    ) {
      // Check if can be placed to the left
      if (getSquare(cell.x - 1, cell.y).is('_')) {
        setValue(cell.x - 1, cell.y, '*')
      }
      // Check if can be placed above
      else if (getSquare(cell.x, cell.y - 1).is('*')) {
        setValue(cell.x, cell.y - 1, '*')
      }
      // Check if can be placec below
      else if (getSquare(cell.x, cell.y + 1).is('*')) {
        setValue(cell.x, cell.y + 1, '*')
      }
    }
    // Above
    else if (getSquare(cell.x, cell.y - 1).is('*') &&
      (getSquare(cell.x - 1, cell.y).isNot('*') || getSquare(cell.x + 1, cell.y).isNot('*') || getSquare(cell.x, cell.y + 1).isNot('*'))
    ) {
      // Check if can be placed to the left
      if (getSquare(cell.x - 1, cell.y).is('_')) {
        setValue(cell.x - 1, cell.y, '*')
      }
      // Check if can be placed above
      else if (getSquare(cell.x, cell.y - 1).is('*')) {
        setValue(cell.x, cell.y - 1, '*')
      }
      // Check if can be placec above
      else if (getSquare(cell.x, cell.y + 1).is('*')) {
        setValue(cell.x, cell.y - 1, '*')
      }
    }
    // Below
    else if (getSquare(cell.x, cell.y + 1).is('*') &&
      (getSquare(cell.x - 1, cell.y).isNot('*') || getSquare(cell.x + 1, cell.y).isNot('*') || getSquare(cell.x, cell.y - 1).isNot('*'))
    ) {
      // Check if can be placed to the left
      if (getSquare(cell.x - 1, cell.y).is('_')) {
        setValue(cell.x - 1, cell.y, '*')
      }
      // Check if can be placed above
      else if (getSquare(cell.x, cell.y - 1).is('*')) {
        setValue(cell.x, cell.y - 1, '*')
      }
      // Check if can be placec below
      else if (getSquare(cell.x, cell.y + 1).is('*')) {
        setValue(cell.x, cell.y + 1, '*')
      }
    }













    // Left + above
    if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x, cell.y - 1).is('_')) {
      if (noLightInColumn(cell.x - 1)) {
        setValue(cell.x - 1, cell.y, '*')
        cellChangeToTilde(cell, cell.x - 1, cell.y)
      }

      if (noLightInRow(cell.y - 1)) {
        setValue(cell.x, cell.y - 1, '*')
        cellChangeToTilde(cell, cell.x, cell.y - 1)
      }
    }

    // Left + below
    if (getSquare(cell.x - 1, cell.y).is('_') && getSquare(cell.x, cell.y + 1).is('_')) {
      if (noLightInColumn(cell.x - 1)) {
        setValue(cell.x - 1, cell.y, '*')
        cellChangeToTilde(cell, cell.x - 1, cell.y)
      }

      if (noLightInRow(cell.y + 1)) {
        setValue(cell.x, cell.y + 1, '*')
        cellChangeToTilde(cell, cell.x, cell.y + 1)
      }
    }

    // Right + above
    if (getSquare(cell.x + 1, cell.y).is('_') && getSquare(cell.x, cell.y - 1).is('_')) {
      if (noLightInColumn(cell.x + 1)) {
        setValue(cell.x + 1, cell.y, '*')
        cellChangeToTilde(cell, cell.x + 1, cell.y)
      }

      if (noLightInRow(cell.y - 1)) {
        setValue(cell.x, cell.y - 1, '*')
        cellChangeToTilde(cell, cell.x, cell.y - 1)
      }
    }

    // Right + below
    if (getSquare(cell.x + 1, cell.y).is('*') && getSquare(cell.x, cell.y + 1).is('_')) {
      if (noLightInColumn(cell.x + 1)) {
        setValue(cell.x + 1, cell.y, '*')
        cellChangeToTilde(cell, cell.x + 1, cell.y)
      }

      if (noLightInRow(cell.y - 1)) {
        setValue(cell.x, cell.y - 1, '*')
        cellChangeToTilde(cell, cell.x, cell.y - 1)
      }
    }
  }

  def fourMatch(cell: Square) = {
    setValue(cell.x + 1, cell.y, '*')
    setValue(cell.x - 1, cell.y, '*')
    setValue(cell.x, cell.y + 1, '*')
    setValue(cell.x, cell.y - 1, '*')
    cellChangeToTilde(cell, cell.x - 1, cell.y) //send left square
    cellChangeToTilde(cell, cell.x + 1, cell.y) //right square
    cellChangeToTilde(cell, cell.x, cell.y - 1) //top square
    cellChangeToTilde(cell, cell.x, cell.y + 1) //bottom square

  }
}