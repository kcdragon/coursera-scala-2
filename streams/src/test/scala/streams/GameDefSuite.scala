package streams

import org.scalatest.FunSuite

import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner

import Bloxorz._

@RunWith(classOf[JUnitRunner])
class GameDefSuite extends FunSuite {


  trait TerrainLevel extends GameDef {
    val goal: Pos = Pos(0,0)
    val startPos: Pos = Pos(0,0)
    val terrain: Terrain = (p: Pos) => true
  }

  trait NoTerrainLevel extends GameDef {
    val goal: Pos = Pos(0,0)
    val startPos: Pos = Pos(0,0)
    val terrain: Terrain = (p: Pos) => false
  }


  trait Level1 extends StringParserTerrain {
    val level =
    """ooo-------
      |oSoooo----
      |ooooooooo-
      |-ooooooooo
      |-----ooToo
      |------ooo-""".stripMargin
  }

  test("isStanding returns true for block that is standing") {
    new TerrainLevel {
      assert(Block(Pos(0,0), Pos(0,0)).isStanding)
    }
  }

  test("isStanding returns false for block that is not standing") {
    new TerrainLevel {
      assert(!Block(Pos(0,0), Pos(0,2)).isStanding)
    }
  }

  test("isLegal returns true for block that is inside the terrain") {
    new TerrainLevel {
      assert(Block(Pos(0,0), Pos(0,0)).isLegal)
    }
  }

  test("isLegal returns false for block that is outside the terrain") {
    new NoTerrainLevel {
      assert(!Block(Pos(0,0), Pos(0,0)).isLegal)
    }
  }

  test("startBlock returns starting block") {
    new Level1 {
      assert(Block(Pos(1, 1), Pos(1, 1)) === startBlock)
    }
  }

  test("neighbors returns surrounding blocks") {
    new Level1 {
      val expectedNeighbors: List[(Block, Move)] = {
        (Block(Pos(3,1), Pos(3, 2)), Left) ::
          (Block(Pos(3,4), Pos(3, 5)), Right) ::
          (Block(Pos(1,3), Pos(2, 3)), Up) ::
          (Block(Pos(4,3), Pos(5, 3)), Down) ::
          Nil
      }
      assert(Block(Pos(3, 3), Pos(3, 3)).neighbors === expectedNeighbors)
    }
  }

  test("legalNeighbors excludes the moves that contain an empty space") {
    new Level1 {
      val expectedLegalNeighbors: List[(Block, Move)] = {
        (Block(Pos(3,1), Pos(3, 2)), Left) ::
          (Block(Pos(3,4), Pos(3, 5)), Right) ::
          (Block(Pos(1,3), Pos(2, 3)), Up) ::
          Nil
      }
      assert(Block(Pos(3, 3), Pos(3, 3)).legalNeighbors === expectedLegalNeighbors)
    }
  }

}
