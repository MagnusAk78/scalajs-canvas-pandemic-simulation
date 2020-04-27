package maak.model.physics2D.shapes

import maak.model.physics2D.Position2D
import utest.{TestSuite, Tests, test, intercept}

object ShapesTest extends TestSuite {

  import maak.model.physics2D.FloatNumComparisions._

  def tests = Tests {
    test("Rectangle2D") {
      val position5050 = Position2D(50, 50)
      val rectangle50 = Rectangle2D(Position2D.origo, position5050)

      test("boundaryBox_min") {
        assert(rectangle50.boundaryBox._1 ~= Position2D.origo)
      }

      test("boundaryBox_max") {
        assert(rectangle50.boundaryBox._2 ~= position5050)
      }

      test("IllegalArgumentException") {
        intercept[IllegalArgumentException] {
          Rectangle2D(Position2D(50, 0), Position2D(0, 50))
        }
      }

      test("createFromTwoPoints") {
        val rect = Rectangle2D.createFromTwoPoints(Position2D(50, 0), Position2D(0, 50))
        assert((rect.boundaryBox._1 ~= Position2D.origo) && (rect.boundaryBox._2 ~= position5050))
      }

      test("getDistanceToEdge_inner") {
        val position2510 = Position2D(25, 10)
        val distanceToEdge = rectangle50.getDistanceToEdge(position2510)

        assert(distanceToEdge ~= -10)
      }

      test("getDistanceToEdge_outer") {
        val positionNeg1010 = Position2D(-10, -10)
        val distanceToEdge = rectangle50.getDistanceToEdge(positionNeg1010)

        assert(distanceToEdge ~= math.sqrt(200))
      }
    }

    test("Circle2D") {
      val radius50 = 50
      val circle50 = Circle2D(Position2D.origo, radius50)

      test("boundaryBox_min") {
        assert(circle50.boundaryBox._1 ~= Position2D(-radius50, -radius50))
      }

      test("boundaryBox_max") {
        assert(circle50.boundaryBox._2 ~= Position2D(radius50, radius50))
      }

      test("area") {
        assert(circle50.area ~= (radius50 * radius50 * math.Pi))
      }

      test("diameter") {
        assert(circle50.diameter ~= (radius50 + radius50))
      }

      test("IllegalArgumentException_radius_0") {
        intercept[IllegalArgumentException] {
          Circle2D(Position2D(50, 0), 0)
        }
      }
    }
  }
}
