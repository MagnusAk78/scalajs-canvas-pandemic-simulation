package maak.model

import scala.util.Random

package object physics2D {

  /**
   * 2D vector.
   *
   * @param x x value of vector
   * @param y y value of vector
   */
  case class Vector2D(x: Double, y: Double) {
    // The magnitude or length of a vector using Pythagoras theorem
    lazy val magnitude: Double = math.sqrt(x*x + y*y)

    // The unit vector is the vector with the same 'direction' but with the magnitude of 1
    lazy val unit: Vector2D = magnitude match {
      case 0.0 => Vector2D.createRandomUnit // Rare case that you want to scale a vector with no magnitude
      case _ => Vector2D(x / magnitude, y / magnitude)
    }

    lazy val inverted: Vector2D = Vector2D(-x, -y)

    lazy val invertX: Vector2D = Vector2D(-x, y)

    lazy val invertY: Vector2D = Vector2D(x, -y)

    def +(other: Vector2D): Vector2D = Vector2D(x + other.x, y + other.y)

    def -(other: Vector2D): Vector2D = Vector2D(x - other.x, y - other.y)

    def *(value: Double): Vector2D = Vector2D(x * value, y * value)

    def scaleTo(newMagnitude: Double): Vector2D = Vector2D(unit.x * newMagnitude, unit.y * newMagnitude)

    def dotProduct(other: Vector2D): Double = x*other.x + y*other.y

    def angleTo(other: Vector2D): Double = {
      val sin = x * other.y - other.x * y
      val cos = x * other.x + y * other.y
      math.atan2(sin, cos)
    }
  }

  object Vector2D {
    def createRandomUnit: Vector2D =
      ((angle: Double) => Vector2D(math.cos(angle), math.sin(angle))).apply(Random.nextDouble() * math.Pi * 2)
  }

  /**
   * The position of a point on a 2D plane.
   *
   * @param x x value of point
   * @param y y value of point
   */
  case class Position2D(x: Double, y: Double) {
    def +(vector: Vector2D): Position2D = Position2D(x + vector.x, y + vector.y)

    def vectorTo(other: Position2D): Vector2D = Vector2D(other.x - x, other.y - y)

    def distanceTo(other: Position2D): Double =
      math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
  }

  object Position2D {
    lazy val origo: Position2D = Position2D(0, 0)
  }
}
