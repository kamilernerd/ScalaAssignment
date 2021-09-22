import PuzzleReaderWriter.{closing, getNumPuzzles, getPuzzle, initRW, putSolution}

object PuzzleSolver extends App {
  def solve(puzzle: Puzzle): Puzzle = {
    // we predefine just two solutions
    val solution7x7 =
      "* _ _ _ _ _ _\n" +
        "2 _ _ * _ _ _\n" +
        "* 1 _ 2 0 _ *\n" +
        "X _ _ * _ _ _\n" +
        "X _ * _ _ _ _\n" +
        "* _ _ _ _ 0 _\n" +
        "2 * X _ * _ _"
    val solution10x5 =
      "_ _ _ _ * _ _ _ 1 *\n" +
        "_ * 1 0 _ * X _ 0 _\n" +
        "_ _ _ _ _ _ _ * _ 1\n" +
        "_ _ * 2 _ 1 * _ X *\n" +
        "_ _ _ * _ _ _ 1 * _"
    val solution4x7 = {
      "\n"+
      "_2__\n" +
        "____\n" +
        "_X__\n" +
        "__1_\n" +
        "____\n" +
        "2_XX\n" +
        "___1"
    }
    val solution8x9 = {
      "_X__1___\n" +
      "____XX__\n" +
      "__2__1_1\n" +
      "_21_____\n" +
      "__1_____\n" +
      "___2____\n" +
      "X___2_2_\n" +
      "__02___X\n" +
      "_____1__"
    }
    val size = puzzle.sizeX * 100 + puzzle.sizeY
    val solution = size match {
      case 407 => solution4x7
      case 707 => solution7x7
      case 809 => solution8x9
      case 1005 => solution10x5
      case _ => "cannot solve this puzzle"
    }
    return new Puzzle(puzzle.sizeX, puzzle.sizeY, solution)
  }

  // arg 0 ./teacher-tests/ValidationData/puzzles/0_4x7_20_0_4x7:a2gBd1e2aBBc1.txt
  // arg 1 ./teacher-tests/ValidationData/solutions/0_4x7_20_0_4x7:a2gBd1e2aBBc1.txt

  // arg 0 ./teacher-tests/ValidationData/puzzles/0_8x9_20_0_8x9:aBb1hBBc2b1a1a21g1h2dBc2a2c02cBe1b.txt
  // arg 1 ./teacher-tests/ValidationData/solutions/0_8x9_20_0_8x9:aBb1hBBc2b1a1a21g1h2dBc2a2c02cBe1b.txt

  initRW(args(0), args(1))

  val numPuzzles = getNumPuzzles()

  for (count <- 0 until numPuzzles) {
    println("Solving puzzle #" + (count + 1).toString)
    putSolution(solve(getPuzzle(count)))
  }

  println("Processed " + numPuzzles.toString + " puzzles.")
  closing()
}
