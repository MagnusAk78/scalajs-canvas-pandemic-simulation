package maak.model.physics2D.shapes

import maak.model.physics2D.Position2D

@throws[IllegalArgumentException]
abstract class RectangleShape(val minPosition: Position2D, val maxPosition: Position2D) extends Shape2D {
  require(maxPosition.x > minPosition.x)
  require(maxPosition.y > minPosition.y)
  override lazy val boundaryBox = (minPosition, maxPosition)
}

/**
 * 2D rectangle.
 * @param minPosition The minimum position (top left in most graphical coordinate systems) of the rectangle.
 * @param maxPosition The maximum position (bottom right in most graphical coordinate systems) of the rectangle.
 */
case class Rectangle2D(override val minPosition: Position2D, override val maxPosition: Position2D)
  extends RectangleShape(minPosition, maxPosition) {

  def inside(position: Position2D): Boolean = {
    val insideX = position.x > minPosition.x && position.x < maxPosition.x
    val insideY = position.y > minPosition.y && position.y < maxPosition.y

    insideX && insideY
  }

  // Closest distance to rectange. Negative number if position is within the rectangle.
  def getEdgePosition(position: Position2D): Position2D = {
    if(inside(position)) {
      // Inside the rectangle (negative value)

      val distLeft = position.x - minPosition.x
      val distRight = maxPosition.x - position.x
      val distTop = position.y - minPosition.y
      val distBottom = maxPosition.y - position.y

      val left = (distLeft < distRight)
      val top = (distTop < distBottom)

      var xPos = 0.0
      var yPos = 0.0

      if(left && top) {
        if(distLeft < distTop) {
          xPos = minPosition.x
          yPos = position.y
        } else {
          xPos = position.x
          yPos = minPosition.y
        }
      } else if(left && !top) {
        if(distLeft < distBottom) {
          xPos = minPosition.x
          yPos = position.y
        } else {
          xPos = position.x
          yPos = maxPosition.y
        }
      } else if(!left && top) {
        if(distRight < distTop) {
          xPos = maxPosition.x
          yPos = position.y
        } else {
          xPos = position.x
          yPos = minPosition.y
        }
      } else {
        if(distRight < distBottom) {
          xPos = maxPosition.x
          yPos = position.y
        } else {
          xPos = position.x
          yPos = maxPosition.y
        }
      }
      Position2D(xPos, yPos)
    } else {
      // Outside the rectangle (positive value)
      val xPos = math.max(minPosition.x, math.min(position.x, maxPosition.x))
      val yPos = math.max(minPosition.y, math.min(position.y, maxPosition.y))
      Position2D(xPos, yPos)
    }
  }

  // Closest distance to rectange. Negative number if position is within the rectangle.
  def getDistanceToEdge(position: Position2D): Double = {
    val edgePos = getEdgePosition(position)
    inside(position) match {
      case true => -edgePos.distanceTo(position)
      case false => edgePos.distanceTo(position)
    }
  }
}

object Rectangle2D {
  def createFromTwoPoints(point1: Position2D, point2: Position2D): Rectangle2D = {
    val minPosition = Position2D(math.min(point1.x, point2.x), math.min(point1.y, point2.y))
    val maxPosition = Position2D(math.max(point1.x, point2.x), math.max(point1.y, point2.y))
    Rectangle2D(minPosition, maxPosition)
  }
}