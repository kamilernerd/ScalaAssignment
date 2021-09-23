class Square(xNumber: Int, yNumber: Int, cellValue: Char) {
  val x: Int = xNumber;
  val y: Int = yNumber;
  val value: Char = cellValue;

  def getValue(): Char = {
    return value
  }

  def setValue(newValue: Char): Square = {
    return new Square(this.x, this.y, newValue);
  }

  //  def findNeighbours(x:Int, y:Int, row: Array[Char]): Square = {
  //    if(x < row.length && x > 0) {
  //      return Array[(x - 1)
  //      , (x + 1)
  //      , (y - 1)
  //      , (y + 1)]
  //    }
  //  }

  override def toString() = {
    "x:" + x + " y:" + y + " value:" + value
  }
}
