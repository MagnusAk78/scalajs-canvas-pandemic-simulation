package maak.model.physics2D.shapes

import maak.model.physics2D._

package object collisions {
  trait Movable[T <: Shape2D] extends Shape2D {
    val velocity: Vector2D

    def move(time: Double): T
  }
}
