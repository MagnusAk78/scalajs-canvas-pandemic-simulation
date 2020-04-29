package maak.model.physics2D.shapes

import maak.model.physics2D._

package object collisions {

  /**
   * Trait describing movable Shape2D.
   *
   * @tparam T The Shape2D that is implemented as movable
   */
  trait Movable[T <: Shape2D] extends Shape2D {
    val velocity: Vector2D

    /** Returns a new Shape2D of type T moved along the velocity vector */
    def move(time: Double): T
  }

}
