package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class SolverSuite extends FunSuite {

  trait Level1 extends Solver with StringParserTerrain {
    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
  }

  test("done returns true if Block is at goal") {
    new Level1 {
      assert(done(Block(Pos(4,7), Pos(4,7))))
    }
  }

}
