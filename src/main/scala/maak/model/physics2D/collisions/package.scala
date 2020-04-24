package maak.model.physics2D

import scala.annotation.tailrec
import scala.scalajs.js

package object collisions {

  abstract class Circle {
    var position: Point2D
    var radius: Double

    def getOverlapWith(other: Circle): Double = {
      (radius + other.radius) - position.distanceTo(other.position)
    }
  }

  class FixedCircle(override var position: Point2D, override var radius: Double) extends Circle

  class MovableCircle(override var position: Point2D, override var radius: Double, var velocity: Vector2D)
    extends Circle {
    def move(time: Double) {
      position = position + velocity * time
    }
  }

  case class CircleSpawner(outerBoundary: OuterBoundary, minSpeed: Double = 10.0, maxSpeed: Double = 10.0,
                               minRadius: Double = 10.0, maxRadius: Double = 10.0, maxSpawnTime: Double = 1.0) {

    private def spawnRandomCircle[T <: Circle](createCircleFunc: () => T)(existingCircles: List[Circle]): Option[T] = {
      @tailrec
      def createRandomCircleInternal(startTime: Double, existingCircles: List[Circle]): Option[T] = {
        val newCircle = createCircleFunc()
        existingCircles.find(circle => newCircle.getOverlapWith(circle) > 0) match {
          case Some(_) =>
            if((js.Date.now() - startTime) < maxSpawnTime) {
              createRandomCircleInternal(startTime, existingCircles)
            } else {
              None
            }
          case None => Some(newCircle)
        }
      }
      createRandomCircleInternal(js.Date.now(), existingCircles)
    }

    def spawnRandomMovableCircle(existingCircles: List[Circle]): Option[MovableCircle] = {
      spawnRandomCircle[MovableCircle](createCircleFunc = () => {
        val radius = Randomizer(minRadius, maxRadius).getValue
        new MovableCircle(
          position = Point2D.createRandom(OuterBoundary(minPosition = outerBoundary.minPosition + Vector2D(radius, radius),
            maxPosition = outerBoundary.maxPosition + Vector2D(-radius, -radius))),
          radius = radius,
          velocity = createRandomUnitVector2D.scale(Randomizer(minSpeed, maxSpeed).getValue))
      })(existingCircles)
    }

    def spawnRandomFixedCircle(existingCircles: List[Circle]): Option[FixedCircle] = {
      spawnRandomCircle[FixedCircle](createCircleFunc = () => {
        val radius = Randomizer(minRadius, maxRadius).getValue
        new FixedCircle(
          position = Point2D.createRandom(OuterBoundary(minPosition = outerBoundary.minPosition + Vector2D(radius, radius),
            maxPosition = outerBoundary.maxPosition + Vector2D(-radius, -radius))),
          radius = radius)
      })(existingCircles)
    }
  }

  def checkForCollisionAndUpdate(circle: MovableCircle, boundary: OuterBoundary): Boolean = {
    if (circle.position.x - circle.radius < boundary.minPosition.x) {
      circle.position = circle.position.copy(x = boundary.minPosition.x + circle.radius)
      circle.velocity = circle.velocity.invertX
      true
    } else if (circle.position.x + circle.radius > boundary.maxPosition.x) {
      circle.position = circle.position.copy(x = boundary.maxPosition.x - circle.radius)
      circle.velocity = circle.velocity.invertX
      true
    } else if (circle.position.y - circle.radius < boundary.minPosition.y) {
      circle.position = circle.position.copy(y = boundary.minPosition.y + circle.radius)
      circle.velocity = circle.velocity.invertY
      true
    } else if (circle.position.y + circle.radius > boundary.maxPosition.y) {
      circle.position = circle.position.copy(y = boundary.maxPosition.y - circle.radius)
      circle.velocity = circle.velocity.invertY
      true
    } else {
      false
    }
  }

  def checkForCollisionAndUpdate(movableCircle: MovableCircle, otherCircle: Circle): Boolean = {
    val overlap = movableCircle.getOverlapWith(otherCircle)
    if (overlap > 0) {
      otherCircle match {
        case otherMovableCircle: MovableCircle => {
          val normalVector1 = otherMovableCircle.position.vectorTo(movableCircle.position)
          val normalVector2 = movableCircle.position.vectorTo(otherMovableCircle.position)

          // Create new positions to remove overlap
          movableCircle.position = movableCircle.position + normalVector1.scale(overlap / 2)
          otherMovableCircle.position = otherMovableCircle.position + normalVector2.scale(overlap / 2)

          val v1_v2 = movableCircle.velocity - otherMovableCircle.velocity
          val v2_v1 = otherMovableCircle.velocity - movableCircle.velocity

          movableCircle.velocity = movableCircle.velocity -
            normalVector1 * (v1_v2.dotProduct(normalVector1) / math.pow(normalVector1.magnitude, 2))
          otherMovableCircle.velocity = otherMovableCircle.velocity -
            normalVector2 * (v2_v1.dotProduct(normalVector2) / math.pow(normalVector2.magnitude, 2))
        }
        case otherFixedCircle: FixedCircle => {
          val normalVector1 = otherFixedCircle.position.vectorTo(movableCircle.position)

          // Create new positions to remove overlap
          movableCircle.position = movableCircle.position + normalVector1.scale(overlap)

          movableCircle.velocity = movableCircle.velocity -
            normalVector1 * (movableCircle.velocity.dotProduct(normalVector1) / math.pow(normalVector1.magnitude, 2)) * 2
        }
      }
      true
    } else {
      false
    }
  }
}
