package maak.model.physics2D.shapes

import maak.model.physics2D.{Position2D, Vector2D}

import scala.util.Random

/** Describes a circle shape
  *
  * @param center Position2D describing the center point of the circle
  * @param radius Radius of the circle
  */
@throws[IllegalArgumentException]
abstract class CircleShape(val center: Position2D, val radius: Double) extends Shape2D {
  require(radius > Double.MinPositiveValue)

  /** Returns the boundary box that precisely fit the circle */
  override lazy val boundaryBox: BoundaryBox = BoundaryBox(center + Vector2D(radius, radius).inverted,
    center + Vector2D(radius, radius))

  /** Returns the diameter of the circle (radius * 2) */
  lazy val diameter: Double = radius * 2

  /** Returns the amount of overlap between this CircleShape and another */
  def getOverlapWith(other: CircleShape): Double = (radius + other.radius) - center.distanceTo(other.center)
}

/** Implementation of the CircleShape
  *
  * @param center Position2D describing the center point of the circle
  * @param radius Radius of the circle
  */
case class Circle2D(override val center: Position2D, override val radius: Double) extends CircleShape(center, radius)

object Circle2D {
  /** Returns a Circle2D with specific radius randomly positioned completely within a given boundary box */
  def createRandom(outerBoundary: BoundaryBox, radius: Double): Circle2D = {
    val center = Position2D(Random.between(outerBoundary.minPosition.x + radius, outerBoundary.maxPosition.x - radius),
      Random.between(outerBoundary.minPosition.y + radius, outerBoundary.maxPosition.y - radius))
    Circle2D(center, radius)
  }
}