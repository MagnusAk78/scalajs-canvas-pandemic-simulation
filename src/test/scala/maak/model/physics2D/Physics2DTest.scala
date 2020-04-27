package maak.model.physics2D

import utest._

object Physics2DTest extends TestSuite {

  import FloatNumComparisions._

  def tests = Tests {
    val vector34 = Vector2D(3, 4)
    test("Vector2D") {
      val vectorNeg34 = Vector2D(-3, -4)

      test("magnitude") {
        assert(vector34.magnitude ~= 5)
      }

      test("unit") {
        assert(Vector2D(34, 25).unit.magnitude ~= 1)
      }

      test("inverted") {
        assert(vector34.inverted ~= vectorNeg34)
      }

      test("scale") {
        assert(vector34.scaleTo(524).magnitude ~= 524)
      }

      test("dot_product") {
        assert(vector34.dotProduct(vector34) ~= 25)
      }

      test("random_unit") {
        assert(Vector2D.createRandomUnit.magnitude ~= 1)
      }

      test("angle_to_same") {
        assert(Vector2D(1,0).angleTo(Vector2D(1,0)) ~= 0)
      }

      test("angle_to_opposite") {
        assert(Vector2D(1,0).angleTo(Vector2D(-1,0)) ~= math.Pi)
      }

      test("angle_to_90deg_1") {
        assert(Vector2D(1,0).angleTo(Vector2D(0,1)) ~= math.Pi/2)
      }

      test("angle_to_90deg_2") {
        assert(Vector2D(1,0).angleTo(Vector2D(0,-1)) ~= -math.Pi/2)
      }
    }

    test("Position2D") {
      val position34 = Position2D(3, 4)
      val position68 = Position2D(6, 8)

      test("add_vector") {
        assert(position34 + vector34 ~= position68)
      }
    }
  }
}
