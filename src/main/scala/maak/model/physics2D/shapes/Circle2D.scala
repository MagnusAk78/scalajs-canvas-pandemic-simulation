package maak.model.physics2D.shapes

import maak.model.physics2D.{Position2D, Vector2D}

import scala.util.Random

@throws[IllegalArgumentException]
abstract class CircleShape(val center: Position2D, val radius: Double) extends Shape2D {
  require(radius > Double.MinPositiveValue)

  override lazy val boundaryBox = BoundaryBox(center + Vector2D(radius, radius).inverted,
    center + Vector2D(radius, radius))

  lazy val diameter: Double = radius * 2

  def getOverlapWith(other: CircleShape): Double = (radius + other.radius) - center.distanceTo(other.center)
}

/**
 * 2D Circle.
 * @param center Center point.
 * @param radius Radius of the circle.
 */
case class Circle2D(override val center: Position2D, override val radius: Double) extends CircleShape(center, radius)

object Circle2D {
  def createRandom(outerBoundary: BoundaryBox, radius: Double): Circle2D = {
    val center = Position2D(Random.between(outerBoundary.minPosition.x + radius, outerBoundary.maxPosition.x - radius),
      Random.between(outerBoundary.minPosition.y + radius, outerBoundary.maxPosition.y - radius))
    Circle2D(center, radius)
  }
}