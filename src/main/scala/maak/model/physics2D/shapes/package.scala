package maak.model.physics2D

package object shapes {

  /** Defines a rectangle in a 2D coordinate system where top and bottom are parallel to the x-axis
    * and left and right sides are parallel to the y-axis. Meaning it can be defined by using only two
    * points in a 2D coordinate system.
    *
    * @param minPosition Minimum position (top-left in most 2D graphics coordinates)
    * @param maxPosition Maximum position (bottom-right in most 2D graphics coordinates)
    */
  @throws[IllegalArgumentException]
  case class BoundaryBox(minPosition: Position2D, maxPosition: Position2D) {
    require(minPosition.x < maxPosition.x && minPosition.y < maxPosition.y)

    /** Returns true if a Position2D describes a point within this BoundaryBox */
    def inside(position: Position2D): Boolean = {
      val insideX = position.x > minPosition.x && position.x < maxPosition.x
      val insideY = position.y > minPosition.y && position.y < maxPosition.y

      insideX && insideY
    }

    /** Returns Position2D that describes the point on the BoundaryBox edge that is closest to given Position2D */
    def getEdgePosition(position: Position2D): Position2D = {
      if (inside(position)) {
        val distLeft = position.x - minPosition.x
        val distRight = maxPosition.x - position.x
        val distTop = position.y - minPosition.y
        val distBottom = maxPosition.y - position.y

        val left = distLeft < distRight
        val top = distTop < distBottom

        val xPos = if (left && distLeft < math.min(distTop, distBottom)) {
          minPosition.x
        } else if (!left && distRight < math.min(distTop, distBottom)) {
          maxPosition.x
        } else {
          position.x
        }

        val yPos = if (top && distTop < math.min(distLeft, distRight)) {
          minPosition.y
        } else if (!top && distBottom < math.min(distLeft, distRight)) {
          maxPosition.y
        } else {
          position.y
        }

        Position2D(xPos, yPos)
      } else {
        val xPos = math.max(minPosition.x, math.min(position.x, maxPosition.x))
        val yPos = math.max(minPosition.y, math.min(position.y, maxPosition.y))
        Position2D(xPos, yPos)
      }
    }
  }

  object BoundaryBox {
    /**
      * Creates a BoundaryBox from any two (different in both x and y axis) Position2D that are not
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

  /** Trait describing any shape in a 2D coordinate system. */
  trait Shape2D {
    /** The boundary box surrounding the shape */
    val boundaryBox: BoundaryBox
  }
}
