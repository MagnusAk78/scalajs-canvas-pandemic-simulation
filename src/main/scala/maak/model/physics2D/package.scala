package maak.model

package object physics2D {

  case class Randomizer(minValue: Double, maxValue: Double) {
    def getValue: Double = minValue + math.random() * (maxValue - minValue)
  }

  case class Vector2D(x: Double, y: Double) {

    // Make sure that the vector has some magnitude to avoid division by zero
    assert(magnitude > 0.0)

    // The magnitude or length of a vector using Pythagoras theorem
    lazy val magnitude: Double = math.sqrt(x*x + y*y)

    // The unit vector is the vector with the same 'direction' but with the magnitude of 1
    lazy val unit: Vector2D = Vector2D(x/magnitude, y/magnitude)

    lazy val inverted: Vector2D = Vector2D(-x, -y)

    lazy val invertX: Vector2D = Vector2D(-x, y)

    lazy val invertY: Vector2D = Vector2D(x, -y)

    def +(other: Vector2D): Vector2D = Vector2D(x + other.x, y + other.y)

    def -(other: Vector2D): Vector2D = Vector2D(x - other.x, y - other.y)

    def *(value: Double): Vector2D = Vector2D(x * value, y * value)

    def scale(newMagnitude: Double): Vector2D = Vector2D(unit.x * newMagnitude, unit.y * newMagnitude)

    def dotProduct(other: Vector2D): Double = x*other.x + y*other.y
  }

  def createRandomUnitVector2D: Vector2D = {
    val angle = math.random() * math.Pi * 2
    Vector2D(math.cos(angle), math.sin(angle))
  }

  case class Point2D(x: Double, y: Double) {
    def +(vector: Vector2D): Point2D = Point2D(x + vector.x, y + vector.y)

    def vectorTo(other: Point2D): Vector2D = Vector2D(other.x - x, other.y - y)

    def distanceTo(other: Point2D): Double =
      math.sqrt(math.pow(x - other.x, 2) + math.pow(y - other.y, 2))
  }

  case class OuterBoundary(minPosition: Point2D, maxPosition: Point2D)

  object Point2D {
    def createRandom(outerBoundary: OuterBoundary): Point2D = {
      val xPos = Randomizer(outerBoundary.minPosition.x, outerBoundary.maxPosition.x).getValue
      val yPos = Randomizer(outerBoundary.minPosition.y, outerBoundary.maxPosition.y).getValue
      Point2D(xPos, yPos)
    }
  }
}
