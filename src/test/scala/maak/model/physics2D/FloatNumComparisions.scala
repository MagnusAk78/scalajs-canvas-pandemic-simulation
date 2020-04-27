package maak.model.physics2D

object FloatNumComparisions {
  implicit class DoubleAddons(value: Double) {
    def ~=(other: Double, precision: Double = 0.001): Boolean = {
      if ((value - other).abs < precision) true else false
    }
  }

  implicit class Vector2DAddons(vector: Vector2D) {
    def ~=(other: Vector2D, precision: Double = 0.001): Boolean = {
      (vector.x ~= (other.x, precision)) && (vector.y ~= (other.y, precision))
    }
  }

  implicit class Position2DAddons(position: Position2D) {
    def ~=(other: Position2D, precision: Double = 0.001): Boolean = {
      (position.x ~= (other.x, precision)) && (position.y ~= (other.y, precision))
    }
  }
}
