package maak.model.physics2D.shapes.collisions

import maak.model.physics2D.shapes.{Circle2D, Rectangle2D}
import maak.model.physics2D.{Position2D, Vector2D}
import utest.{TestSuite, Tests, intercept, test}

object CollisionsTest extends TestSuite {

  import maak.model.physics2D.FloatNumComparisions._

  def tests = Tests {

    test("MovableCircle2D") {
      test("collision_outer_boundary") {
        val position100200 = Position2D(100, 200)
        val outerBoundary100 = Rectangle2D(Position2D.origo, position100200)
        val radius20 = 20

        test("reposition_min") {
          val beforeCollision = MovableCircle2D(Position2D.origo, radius20, Vector2D(-1, 0))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.center ~= Position2D(radius20, radius20))
        }

        test("reposition_max") {
          val beforeCollision = MovableCircle2D(position100200, radius20, Vector2D(-1, 0))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.center ~= Position2D(position100200.x - radius20, position100200.y - radius20))
        }

        test("min_x") {
          val beforeCollision = MovableCircle2D(Position2D.origo, radius20, Vector2D(-1, 0))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(1, 0))
        }

        test("min_y") {
          val beforeCollision = MovableCircle2D(Position2D.origo, radius20, Vector2D(0, -1))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(0, 1))
        }

        test("max_x") {
          val beforeCollision = MovableCircle2D(position100200, radius20, Vector2D(1, 0))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(-1, 0))
        }

        test("max_y") {
          val beforeCollision = MovableCircle2D(position100200, radius20, Vector2D(0, 1))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(0, -1))
        }

        test("min_x_inverted_velocity") {
          val beforeCollision = MovableCircle2D(Position2D.origo, radius20, Vector2D(1, 0))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(1, 0))
        }

        test("min_y_inverted_velocity") {
          val beforeCollision = MovableCircle2D(Position2D.origo, radius20, Vector2D(0, 1))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(0, 1))
        }

        test("max_x_inverted_velocity") {
          val beforeCollision = MovableCircle2D(position100200, radius20, Vector2D(-1, 0))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(-1, 0))
        }

        test("max_y_inverted_velocity") {
          val beforeCollision = MovableCircle2D(position100200, radius20, Vector2D(0, -1))
          val afterCollision = MovableCircle2D.checkCollision(beforeCollision, outerBoundary100)
          assert(afterCollision.get.velocity ~= Vector2D(0, -1))
        }
      }

      test("collision_Circle2D") {
        val velRight = Vector2D(1,0)
        val velLeft = Vector2D(-1,0)
        val velUp = Vector2D(0,-1)
        val velDown = Vector2D(0,1)
        val radius50 = 50
        val positionLeft = Position2D(Position2D.origo.x - (radius50 * 2 - 1), Position2D.origo.y)
        val positionBottom = Position2D(Position2D.origo.x, Position2D.origo.y + (radius50 * 2 - 1))
        val movableCircleLeftToRight = MovableCircle2D(positionLeft, radius50, velRight)
        val movableCircleLeftToLeft = MovableCircle2D(positionLeft, radius50, velLeft)
        val movableCircleBottomToUp = MovableCircle2D(positionBottom, radius50, velUp)
        val movableCircleBottomToDown = MovableCircle2D(positionBottom, radius50, velDown)
        val fixedCircleCenter = Circle2D(Position2D.origo, radius50)

        test("left_to_right_reposition") {
          val afterCollision = MovableCircle2D.checkCollision(movableCircleLeftToRight, fixedCircleCenter)
          assert(afterCollision.get.center ~= Position2D(Position2D.origo.x - (radius50 * 2), Position2D.origo.y))
        }

        test("left_to_right_velocity") {
          val afterCollision = MovableCircle2D.checkCollision(movableCircleLeftToRight, fixedCircleCenter)
          assert(afterCollision.get.velocity ~= velLeft)
        }

        test("left_to_left_velocity") {
          val afterCollision = MovableCircle2D.checkCollision(movableCircleLeftToLeft, fixedCircleCenter)
          assert(afterCollision.get.velocity ~= velLeft)
        }

        test("bottom_to_up_reposition") {
          val afterCollision = MovableCircle2D.checkCollision(movableCircleBottomToUp, fixedCircleCenter)
          assert(afterCollision.get.center ~= Position2D(Position2D.origo.x, Position2D.origo.y + (radius50 * 2)))
        }

        test("bottom_to_up_velocity") {
          val afterCollision = MovableCircle2D.checkCollision(movableCircleBottomToUp, fixedCircleCenter)
          assert(afterCollision.get.velocity ~= velDown)
        }

        test("bottom_to_down_velocity") {
          val afterCollision = MovableCircle2D.checkCollision(movableCircleBottomToDown, fixedCircleCenter)
          assert(afterCollision.get.velocity ~= velDown)
        }
      }

      test("collision_MovableCircle2D") {
        val velRight = Vector2D(1,0)
        val velLeft = Vector2D(-1,0)
        val velLeftFast = Vector2D(-2,0)
        val velUp = Vector2D(0,-1)
        val velDown = Vector2D(0,1)
        val radius50 = 50
        val positionLeft = Position2D(Position2D.origo.x - (radius50 * 2 - 2), Position2D.origo.y)
        val positionBottom = Position2D(Position2D.origo.x, Position2D.origo.y + (radius50 * 2 - 2))
        val movableCircleLeftToRight = MovableCircle2D(positionLeft, radius50, velRight)
        val movableCircleLeftToLeft = MovableCircle2D(positionLeft, radius50, velLeft)
        val movableCircleCentertoLeft = MovableCircle2D(Position2D.origo, radius50, velLeft)
        val movableCircleCentertoLeftFast = MovableCircle2D(Position2D.origo, radius50, velLeftFast)

        test("opposition_from_left_reposition") {
          val collisionTuple = MovableCircle2D.checkCollision(movableCircleLeftToRight, movableCircleCentertoLeft)
          assert(collisionTuple.get._1.center ~= Position2D(Position2D.origo.x - (radius50 * 2 - 1), Position2D.origo.y))
          assert(collisionTuple.get._2.center ~= Position2D(Position2D.origo.x + 1, Position2D.origo.y))
        }

        test("opposition_from_left_velocity") {
          val collisionTuple = MovableCircle2D.checkCollision(movableCircleLeftToRight, movableCircleCentertoLeft)
          assert(collisionTuple.get._1.velocity ~= velLeft)
          assert(collisionTuple.get._2.velocity ~= velRight)
        }

        test("opposition_from_left_velocity_catching_up") {
          val collisionTuple = MovableCircle2D.checkCollision(movableCircleLeftToLeft, movableCircleCentertoLeftFast)
          println(collisionTuple)
          assert(collisionTuple.get._1.velocity ~= velLeftFast)
          assert(collisionTuple.get._2.velocity ~= velLeft)
        }
      }
    }
  }
}