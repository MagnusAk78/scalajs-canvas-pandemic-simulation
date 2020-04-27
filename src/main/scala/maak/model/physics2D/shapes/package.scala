package maak.model.physics2D

import scala.util.Random

package object shapes {
  trait Shape2D {
    val boundaryBox: (Position2D, Position2D)
  }
}
