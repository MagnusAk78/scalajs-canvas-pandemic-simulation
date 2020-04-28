package maak.model.physics2D

package object shapes {

  /**
   * Defines a rectangle in a 2D coordinate system where top and bottom are parallel to the x-axis
   * and left and right sides are parallel to the y-axis.
   *
   * @param minPosition Minimum position (top-left in most 2D graphics coordinates)
   * @param maxPosition Maximum position (bottom-right in most 2D graphics coordinates)
   */
  case class BoundaryBox(minPosition: Position2D, maxPosition: Position2D) {
    require(minPosition.x < maxPosition.x && minPosition.y < maxPosition.y)

    def inside(position: Position2D): Boolean = {
      val insideX = position.x > minPosition.x && position.x < maxPosition.x
      val insideY = position.y > minPosition.y && position.y < maxPosition.y

      insideX && insideY
    }

    // Closest distance to BoundaryBox. Negative number if position is within the rectangle.
    def getEdgePosition(position: Position2D): Position2D = {
      if (inside(position)) {
        // Inside the rectangle (negative value)

        val distLeft = position.x - minPosition.x
        val distRight = maxPosition.x - position.x
        val distTop = position.y - minPosition.y
        val distBottom = maxPosition.y - position.y

        val left = (distLeft < distRight)
        val top = (distTop < distBottom)

        var xPos = 0.0
        var yPos = 0.0

        if (left && top) {
          if (distLeft < distTop) {
            xPos = minPosition.x
            yPos = position.y
          } else {
            xPos = position.x
            yPos = minPosition.y
          }
        } else if (left && !top) {
          if (distLeft < distBottom) {
            xPos = minPosition.x
            yPos = position.y
          } else {
            xPos = position.x
            yPos = maxPosition.y
          }
        } else if (!left && top) {
          if (distRight < distTop) {
            xPos = maxPosition.x
            yPos = position.y
          } else {
            xPos = position.x
            yPos = minPosition.y
          }
        } else {
          if (distRight < distBottom) {
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

    // Closest distance to BoundaryBox. Negative number if position is within the rectangle.
    def getDistanceToEdge(position: Position2D): Double = {
      val edgePos = getEdgePosition(position)
      inside(position) match {
        case true => -edgePos.distanceTo(position)
        case false => edgePos.distanceTo(position)
      }
    }
  }

  object BoundaryBox {
    /**
     * Creates a Boundary Box from any two (different in both x and y axis) positions that is not
     * necessarily top/left and bottom/right.
     *
     * @param position1 One corner of the bounding box.
     * @param position2 The opposite corner of the bounding box.
     */
    def createBoundaryBox(position1: Position2D, position2: Position2D): BoundaryBox = {
      val minPosition = Position2D(math.min(position1.x, position2.x), math.min(position1.y, position2.y))
      val maxPosition = Position2D(math.max(position1.x, position2.x), math.max(position1.y, position2.y))
      BoundaryBox(minPosition, maxPosition)
    }
  }

  trait Shape2D {
    val boundaryBox: BoundaryBox
  }
}
