package maak.model.physics2D.shapes

import maak.model.physics2D.Position2D
import utest.{TestSuite, Tests, test, intercept}

object ShapesTest extends TestSuite {

  import maak.model.physics2D.FloatNumComparisions._

  def tests = Tests {
    test("BoundaryBox") {
      val position5050 = Position2D(50, 50)
      val boundaryBox50 = BoundaryBox(Position2D.origo, position5050)

      test("min") {
        assert(boundaryBox50.minPosition ~= Position2D.origo)
      }

      test("max") {
        assert(boundaryBox50.maxPosition ~= position5050)
      }

      test("IllegalArgumentException") {
        intercept[IllegalArgumentException] {
          BoundaryBox(Position2D(50, 0), Position2D(0, 50))
        }
      }

      test("createBoundaryBox") {
        val bb = BoundaryBox.createBoundaryBox(Position2D(50, 0), Position2D(0, 50))
        assert((bb.minPosition ~= Position2D.origo) && (bb.maxPosition ~= position5050))
      }

      test("getEdgePosition_inner") {
        val position2510 = Position2D(25, 10)
        val edgePos = boundaryBox50.getEdgePosition(position2510)
        val position250 = Position2D(25, 0)

        assert(edgePos ~= position250)
      }

      test("getDistanceToEdge_outer") {
        val positionNeg1010 = Position2D(-10, -10)
        val edgePos = boundaryBox50.getEdgePosition(positionNeg1010)

        assert(edgePos ~= Position2D.origo)
      }
    }

    test("Circle2D") {
      val radius50 = 50
      val circle50 = Circle2D(Position2D.origo, radius50)

      test("boundaryBox_min") {
        assert(circle50.boundaryBox.minPosition ~= Position2D(-radius50, -radius50))
      }

      test("boundaryBox_max") {
        assert(circle50.boundaryBox.maxPosition ~= Position2D(radius50, radius50))
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
