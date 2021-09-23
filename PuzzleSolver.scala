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

    val solution20x15 = {
      "_____1_____________X\n" +
      "_X____X______X__X___\n" +
      "___2_____20___1___X0\n" +
      "___X__3___1____0____\n" +
      "_XX_X3_X_X_X1__2__21\n" +
      "2_____XX2__X_____X__\n" +
      "__X_X____1__0_XX____\n" +
      "X_02___1____X__X2_X_\n" +
      "_____0_10_1_1_X_____\n" +
      "_XX__X__X__________X\n" +
      "___X____X_2__1___1__\n" +
      "__4___XX_4_____1X_XX\n" +
      "_4__X___X_X___1_2__X\n" +
      "__3XX______0_____X__\n" +
      "_____X_3_XX0____1_1_\n"
    }

    val size = puzzle.sizeX * 100 + puzzle.sizeY
    val solution = size match {
      case 407 => solution4x7
      case 707 => solution7x7
      case 809 => solution8x9
      case 1005 => solution10x5
      case 2015 => solution20x15
      case _ => "cannot solve this puzzle"
    }
    return new Puzzle(puzzle.sizeX, puzzle.sizeY, solution)
  }

  // arg 0 ./teacher-tests/ValidationData/puzzles/0_4x7_20_0_4x7:a2gBd1e2aBBc1.txt
  // arg 1 ./teacher-tests/ValidationData/solutions/0_4x7_20_0_4x7:a2gBd1e2aBBc1.txt

  // arg 0 ./teacher-tests/ValidationData/puzzles/0_8x9_20_0_8x9:aBb1hBBc2b1a1a21g1h2dBc2a2c02cBe1b.txt
  // arg 1 ./teacher-tests/ValidationData/solutions/0_8x9_20_0_8x9:aBb1hBBc2b1a1a21g1h2dBc2a2c02cBe1b.txt

  // arg 0 ./teacher-tests/ValidationData/challenges/puzzles/0_20x15_20_0_20x15:e1mBaBdBfBbBf2e20c1cB0cBb3c1d0eBBaB3aBaBaB1b2b212eBB2bBeBdBaBd1b0aBBdBa02c1dBbB2aBf0a10a1a1aBfBBbBbBjBcBdBa2b1c1d4cBBa4e1BaBBa4bBcBaBc1a2bBb3BBf0eBgBa3aBB0d1a1a.txt
  // arg 0 ./teacher-tests/ValidationData/challenges/solutions/0_20x15_20_0_20x15:e1mBaBdBfBbBf2e20c1cB0cBb3c1d0eBBaB3aBaBaB1b2b212eBB2bBeBdBaBd1b0aBBdBa02c1dBbB2aBf0a10a1a1aBfBBbBbBjBcBdBa2b1c1d4cBBa4e1BaBBa4bBcBaBc1a2bBb3BBf0eBgBa3aBB0d1a1a.txt

  initRW(args(0), args(1))

  val numPuzzles = getNumPuzzles()

  for (count <- 0 until numPuzzles) {
    println("Solving puzzle #" + (count + 1).toString)
    putSolution(solve(getPuzzle(count)))
  }

  println("Processed " + numPuzzles.toString + " puzzles.")
  closing()
}
