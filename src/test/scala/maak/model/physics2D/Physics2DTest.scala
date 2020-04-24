package maak.model.physics2D

import utest._

object Physics2DTest extends TestSuite {

  implicit class DoubleAddons(value: Double) {
    def ~=(other: Double, precision: Double = 0.001): Boolean = {
      if ((value - other).abs < precision) true else false
    }
  }

  implicit class Vector2DAddons(vector: Vector2D) {
    def ~=(other: Vector2D, precision: Double = 0.001): Boolean = {
      (vector.x ~= (other.x, precision)) && (vector.y ~= (other.y, precision))
    }
  }

  def tests = Tests {
    test("Vector2D magnitude") {
      assert(Vector2D(3.0, 4.0).magnitude ~= 5.0)
    }

    test("Vector2D unit") {
      assert(Vector2D(34.0, 25.0).inverted ~= Vector2D(-34.0, -25.0))
    }

    test("Vector2D inverted") {
      assert(Vector2D(34.0, 25.0).unit.magnitude ~= 1.0)
    }

    test("Vector2D scale") {
      assert(Vector2D(3.0, 4.0).scale(524.0).magnitude ~= 524.0)
    }

    test("Vector2D dot product") {
      assert(Vector2D(3.0, 4.0).dotProduct(Vector2D(3.0, 4.0)) ~= 25.0)
    }

    test("Vector2D dot product2") {
      assert(Vector2D(5.0, 2.0).dotProduct(Vector2D(4.0, 7.0)) ~= 34.0)
    }

    test("Vector2D random unit") {
      assert(Vector2D.createRandomUnit.magnitude ~= 1.0)
    }
  }
}
