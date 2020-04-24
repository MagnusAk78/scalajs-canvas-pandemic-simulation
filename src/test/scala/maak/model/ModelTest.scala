package maak.model

import utest.{TestSuite, Tests, assert, test}

object ModelTest extends TestSuite {

  def tests = Tests {
    test("Vector2D magnitude") {
      assert(Vector2D(1.0, 0.0).magnitude == 1.0)
    }
  }
}
