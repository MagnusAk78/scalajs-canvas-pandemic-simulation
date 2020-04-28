package maak.webapp.simulation

import maak.model.physics2D.{Position2D, Vector2D}
import maak.model.physics2D.shapes.{Circle2D, CircleShape, BoundaryBox}
import maak.model.physics2D.shapes.collisions.MovableCircle2D

import scala.annotation.tailrec
import scala.scalajs.js

sealed abstract class Entity {
  val infectionTimeSeconds: Double
  val immuneTimeSeconds: Double
  var infected: Boolean = false
  var immune: Boolean = false
  var timeInfected: Double = 0.0
  var timeImmune: Double = 0.0

  def infect():Unit = {
    if (!infected && !immune) {
      infected = true
      timeInfected = js.Date.now()
    }
  }

  def update():Unit = {
    if (infected && (js.Date.now() - timeInfected) / 1000 > infectionTimeSeconds) {
      infected = false
      immune = true
      timeImmune = js.Date.now()
    }

    if (immune && (js.Date.now() - timeImmune) / 1000 > immuneTimeSeconds) {
      immune = false
    }
  }
}

case class FixedEntity(body: Circle2D, override val infectionTimeSeconds: Double,
                       override val immuneTimeSeconds: Double) extends Entity

case class MovableEntity(var body: MovableCircle2D, override val infectionTimeSeconds: Double,
                         override val immuneTimeSeconds: Double) extends Entity

class EntitySpawner {
  var outerBoundary: BoundaryBox = BoundaryBox(Position2D.origo, Position2D(1, 1))
  var infectionTimeSeconds: Double = 1
  var immuneTimeSeconds: Double = 1
  var radius: Double = 1
  var minSpeed: Double = 0
  var maxSpeed: Double = 1
  var numberFixedToSpawn: Int = 0
  var numberMovableToSpawn: Int = 0
  var numberMovableInfected: Int = 1
  var maxSpawnTimeInSeconds: Double = 10.0

  def withOuterBoundary(outerBoundary: BoundaryBox): EntitySpawner = {
    this.outerBoundary = outerBoundary
    this
  }

  def withInfectionTimeSeconds(infectionTimeSeconds: Double): EntitySpawner = {
    this.infectionTimeSeconds = infectionTimeSeconds
    this
  }

  def withImmuneTimeSeconds(immuneTimeSeconds: Double): EntitySpawner = {
    this.immuneTimeSeconds = immuneTimeSeconds
    this
  }

  def withRadius(radius: Double): EntitySpawner = {
    this.radius = radius
    this
  }

  def withMaxSpeed(maxSpeed: Double): EntitySpawner = {
    this.maxSpeed = maxSpeed
    this
  }

  def withNumberFixedToSpawn(numberFixedToSpawn: Int): EntitySpawner = {
    this.numberFixedToSpawn = numberFixedToSpawn
    this
  }

  def withNumberMovableToSpawn(numberMovableToSpawn: Int): EntitySpawner = {
    this.numberMovableToSpawn = numberMovableToSpawn
    this
  }

  // Optional parameters
  def withMinSpeed(minSpeed: Double): EntitySpawner = {
    this.minSpeed = minSpeed
    this
  }

  def withNumberMovableInfected(numberMovableInfected: Int): EntitySpawner = {
    this.numberMovableInfected = numberMovableInfected
    this
  }

  def withMaxSpawnTimeInSeconds(maxSpawnTimeInSeconds: Double): EntitySpawner = {
    this.maxSpawnTimeInSeconds = maxSpawnTimeInSeconds
    this
  }

  def spawn(): (List[FixedEntity], List[MovableEntity]) = {
    @tailrec
    def spawnHelper(spawnedFixed: List[FixedEntity], spawnedMovable: List[MovableEntity],
                    startTime: Double): (List[FixedEntity], List[MovableEntity]) = {
      val diffTimeSeconds = (js.Date.now() - startTime) / 1000
      if (diffTimeSeconds > maxSpawnTimeInSeconds) {
        (spawnedFixed, spawnedMovable)
      } else {

        if (spawnedFixed.length < numberFixedToSpawn) {
          val newCircle = Circle2D.createRandom(outerBoundary, radius)
          spawnedFixed.find(_.body.getOverlapWith(newCircle) > 0) match {
            case Some(_) => spawnHelper(spawnedFixed, spawnedMovable, startTime)
            case None => spawnHelper(FixedEntity(newCircle, infectionTimeSeconds, immuneTimeSeconds) :: spawnedFixed,
              spawnedMovable, startTime)
          }
        } else if (spawnedMovable.length < numberMovableToSpawn) {
          val newMovableCircle = MovableCircle2D.createRandom(outerBoundary, radius, minSpeed, maxSpeed)
          val allCircles: List[CircleShape] = spawnedFixed.map(_.body) ::: spawnedMovable.map(_.body)
          allCircles.find(_.getOverlapWith(newMovableCircle) > 0) match {
            case Some(_) => spawnHelper(spawnedFixed, spawnedMovable, startTime)
            case None => spawnHelper(spawnedFixed,
              MovableEntity(newMovableCircle, infectionTimeSeconds, immuneTimeSeconds) :: spawnedMovable, startTime)
          }
        } else {
          (spawnedFixed, spawnedMovable)
        }
      }
    }

    val (spawnedFixed, spawnedMovable) = spawnHelper(List(), List(), js.Date.now())
    spawnedMovable.take(math.min(numberMovableInfected, spawnedMovable.length)).foreach(_.infect())
    (spawnedFixed, spawnedMovable)
  }
}