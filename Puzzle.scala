// Puzzle class represents the problem and also the solution
class Puzzle(x: Int, y: Int, sol: String) { // just trivial data here
  val sizeX = x;
  val sizeY = y;
  val solution = sol;
  val solutionArray = solution.split("\n")

  var gameBoard = List[Square]();

  def walk(): Puzzle = {

    println(solution)

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
      if (cell.x == 0 && cell.y == 0) {
        setValue(cell.x, cell.y, '*')
      }
    })

    return new Puzzle(x, y, solution)
  }

  def getSquare(x: Int,y: Int): Square = {
    return gameBoard.filter(_.x == x).filter(_.y == y)(0)
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
}
