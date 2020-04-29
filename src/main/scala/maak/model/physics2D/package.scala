package maak.model

import scala.util.{Random, Try}

package object physics2D {

  /**
   * Describes a vector in a 2D coordinate system.
   *
   * @param x x-value
   * @param y y-value
   */
  case class Vector2D(x: Double, y: Double) {
    /** Returns the magnitude (length) of a vector */
    lazy val magnitude: Double = math.sqrt(x * x + y * y)

    /** Returns the unit vector. It is a Vector2D with the same 'direction' as this but with a magnitude of 1 */
    lazy val unit: Vector2D = Vector2D(x / magnitude, y / magnitude)

    /** Returns a new Vector2D with the x and y values inverted compared to this Vector2D */
    lazy val inverted: Vector2D = Vector2D(-x, -y)

    /** Returns a new Vector2D with the x value inverted compared to this Vector2D */
    lazy val invertX: Vector2D = Vector2D(-x, y)

    /** Returns a new Vector2D with the y value inverted compared to this Vector2D */
    lazy val invertY: Vector2D = Vector2D(x, -y)

    /** Returns a new Vector2D which is the sum of this Vector2D and another */
    def +(other: Vector2D): Vector2D = Vector2D(x + other.x, y + other.y)

    /** Returns a new Vector2D which is the subtraction between this Vector2D and another */
    def -(other: Vector2D): Vector2D = Vector2D(x - other.x, y - other.y)

    /** Returns a new Vector2D with x and y multiplied with a given value */
    def *(value: Double): Vector2D = Vector2D(x * value, y * value)

    /** Returns a new Vector2D with a specific magnitude (length) */
    def scaleTo(newMagnitude: Double): Vector2D = Vector2D(unit.x * newMagnitude, unit.y * newMagnitude)

    /** Returns the dot product of two Vector2D (x1 * x2 + y1 * y2) */
    def dotProduct(other: Vector2D): Double = x * other.x + y * other.y

    /** Returns the angle between this Vector2D and another (between -Pi and Pi) */
    def angleTo(other: Vector2D): Double = {
      val sin = x * other.y - other.x * y
      val cos = x * other.x + y * other.y
      math.atan2(sin, cos)
    }
  }

  /**
   * Describes the position of a point in a 2D coordinate system.
   *
   * @param x x-value
   * @param y y-value
   */
  case class Position2D(x: Double, y: Double) {
    /** Returns a new Position2D with an added Vector2D */
    def +(vector: Vector2D): Position2D = Position2D(x + vector.x, y + vector.y)

    /** Returns the Vector2D describing the direction and length to another Position2D */
    def vectorTo(other: Position2D): Vector2D = Vector2D(other.x - x, other.y - y)

    /** Returns the distance to another Position2D */
    def distanceTo(other: Position2D): Double =
      math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
  }

  object Vector2D {
    /** Returns a unit Vector2D with a random 'direction' */
    def createRandomUnit: Vector2D =
      ((angle: Double) => Vector2D(math.cos(angle), math.sin(angle))).apply(Random.nextDouble() * math.Pi * 2)
  }

  object Position2D {
    /** The center position, Position2D(0, 0) */
    lazy val origo: Position2D = Position2D(0, 0)
  }
}
