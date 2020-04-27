package maak.model.physics2D.shapes.collisions

import maak.model.physics2D.shapes._
import maak.model.physics2D._

case class MovableCircle2D(override val center: Position2D, override val radius: Double,
                           override val velocity: Vector2D) extends CircleShape(center, radius)
  with Movable[MovableCircle2D] {
  override def move(time: Double): MovableCircle2D = copy(center = center + velocity * time)
}

object MovableCircle2D {
  def checkCollision(movableCircle: MovableCircle2D,
                     outerBoundary: Rectangle2D): Option[MovableCircle2D] = {

    val edgePos = outerBoundary.getEdgePosition(movableCircle.center)
    val distance = edgePos.distanceTo(movableCircle.center)

    if (distance < movableCircle.radius) {
      // Collision

      // Move circle to fit within outer boundary
      val radiusVector = Vector2D(movableCircle.radius, movableCircle.radius)
      val centerBoundary = Rectangle2D(outerBoundary.minPosition + radiusVector,
        outerBoundary.maxPosition + radiusVector.inverted)
      val newCenter = centerBoundary.getEdgePosition(movableCircle.center)

      val xDiff = movableCircle.center.x - newCenter.x
      val yDiff = movableCircle.center.y - newCenter.y

      var newVelocity = movableCircle.velocity
      if (xDiff < 0 && movableCircle.velocity.x < 0 || xDiff > 0 && movableCircle.velocity.x > 0) {
        newVelocity = movableCircle.velocity.invertX
      }
      if (yDiff < 0 && movableCircle.velocity.y < 0 || yDiff > 0 && movableCircle.velocity.y > 0) {
        newVelocity = movableCircle.velocity.invertY
      }

      Some(movableCircle.copy(center = newCenter, velocity = newVelocity))
    } else {
      // No collision
      None
    }
  }

  def checkCollision(movableCircle1: MovableCircle2D,
                     movableCircle2: MovableCircle2D): Option[(MovableCircle2D, MovableCircle2D)] = {
    val overlap = movableCircle1.getOverlapWith(movableCircle2)
    if (overlap > 0) {
      val normalVector1 = movableCircle2.center.vectorTo(movableCircle1.center)
      val normalVector2 = movableCircle1.center.vectorTo(movableCircle2.center)

      // Create new positions to remove overlap
      val newPosition1 = movableCircle1.center + normalVector1.scaleTo(overlap / 2)
      val newPosition2 = movableCircle2.center + normalVector2.scaleTo(overlap / 2)

      val v1_v2 = movableCircle1.velocity - movableCircle2.velocity
      val newVelocity1 = movableCircle1.velocity - normalVector1 *
            (v1_v2.dotProduct(normalVector1) / math.pow(normalVector1.magnitude, 2))

      val v2_v1 = movableCircle2.velocity - movableCircle1.velocity
      val newVelocity2 = movableCircle2.velocity - normalVector2 *
        (v2_v1.dotProduct(normalVector2) / math.pow(normalVector2.magnitude, 2))

      Some(
        movableCircle1.copy(center = newPosition1, velocity = newVelocity1),
        movableCircle2.copy(center = newPosition2, velocity = newVelocity2)
      )
    } else {
      None
    }
  }

  def checkCollision(movableCircle: MovableCircle2D, otherCircle: Circle2D): Option[MovableCircle2D] = {
    val overlap = movableCircle.getOverlapWith(otherCircle)
    if (overlap > 0) {
      val normalVector = otherCircle.center.vectorTo(movableCircle.center)

      // Create new positions to remove overlap
      val newPosition = movableCircle.center + normalVector.scaleTo(overlap)

      //Make sure the circle is moving against the other circle
      val angle = movableCircle.velocity.angleTo(normalVector)
      val newVelocity = if (angle < -math.Pi / 2 || angle > math.Pi / 2) {
        movableCircle.velocity -
          normalVector * (movableCircle.velocity.dotProduct(normalVector) / math.pow(normalVector.magnitude, 2)) * 2
      } else {
        movableCircle.velocity
      }

      Some(movableCircle.copy(center = newPosition, velocity = newVelocity))
    } else {
      None
    }
  }
}