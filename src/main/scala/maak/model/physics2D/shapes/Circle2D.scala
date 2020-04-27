package maak.model.physics2D.shapes

import maak.model.physics2D.{Position2D, Vector2D}

import scala.util.Random

@throws[IllegalArgumentException]
abstract class CircleShape(val center: Position2D, val radius: Double) extends Shape2D {
  require(radius > Double.MinPositiveValue)

  override lazy val boundaryBox = (center + Vector2D(radius, radius).inverted, center + Vector2D(radius, radius))

  lazy val diameter: Double = radius * 2

  lazy val area: Double = radius * radius * math.Pi

  def getOverlapWith(other: CircleShape): Double = (radius + other.radius) - center.distanceTo(other.center)
}

/**
 * 2D Circle.
 * @param center Center point.
 * @param radius Radius of the circle.
 */
case class Circle2D(override val center: Position2D, override val radius: Double) extends CircleShape(center, radius)

object Circle2D {
  def createRandom(outerBoundary: Rectangle2D, radius: Double): Circle2D = {
    val radiusVector = Vector2D(radius, radius)
    val circleBoundary = Rectangle2D(outerBoundary.minPosition + radiusVector, outerBoundary.maxPosition +
      radiusVector.inverted)
    Circle2D(Position2D.createRandom(circleBoundary.minPosition, circleBoundary.maxPosition), radius)
  }
}