package maak.webapp.simulation

import maak.model.physics2D.{Position2D, Vector2D}
import maak.model.physics2D.shapes.{Circle2D, CircleShape, BoundaryBox}
import maak.model.physics2D.shapes.collisions.MovableCircle2D

import scala.annotation.tailrec
import scala.scalajs.js

sealed abstract class SimulationState
case object Normal extends SimulationState
case object Infected extends SimulationState
case object Immune extends SimulationState

/** Describes an entity in the simulation. */
sealed abstract class Entity {
  /** The time an entity is infected */
  protected val infectionTimeSeconds: Double
  /** The time an entity is immune after infection */
  protected val immuneTimeSeconds: Double
  /** Entity state (normal, infected, immune) */
  var state: SimulationState = Normal
  /** The time the entity state changed */
  private var timeStateChanged: Double = 0.0
  /** A function that is called whenever a state is changed */
  protected val stateChangedFunction: (SimulationState, SimulationState) => Unit

  /** Infect this entity */
  def infect(): Unit = state match {
    case Normal => {
      state = Infected
      timeStateChanged = js.Date.now() / 1000
      stateChangedFunction(Normal, Infected)
    }
    case _ => //Do nothing
  }

  /** Update infection and immune state depending on time */
  def update(): Unit = {
    val nowSeconds = js.Date.now() / 1000
    val timeSinceStateChange = nowSeconds - timeStateChanged

    state match {
      case Infected => {
        if (timeSinceStateChange > infectionTimeSeconds) {
          state = Immune
          timeStateChanged = nowSeconds
          stateChangedFunction(Infected, Immune)
        }
      }
      case Immune => {
        if (state == Immune && timeSinceStateChange > immuneTimeSeconds) {
          state = Normal
          timeStateChanged = nowSeconds
          stateChangedFunction(Immune, Normal)
        }
      }
      case _ => //Do nothing
    }
  }
}

/** Describes a fixed entity in the simulation represented by a non movable CircleShape body (Circle2D) */
case class FixedEntity(body: Circle2D, override val infectionTimeSeconds: Double,
                       override val immuneTimeSeconds: Double,
                       override val stateChangedFunction: (SimulationState, SimulationState) => Unit) extends Entity

/** Describes a movable entity in the simulation represented by a movable CircleShape body (MovableCircle2D) */
case class MovableEntity(var body: MovableCircle2D, override val infectionTimeSeconds: Double,
                         override val immuneTimeSeconds: Double,
                         override val stateChangedFunction: (SimulationState, SimulationState) => Unit) extends Entity

/**
 * A builder for entities. It spawns new randomly generated entities according to certain criteria.
 */
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
  var stateChangedFunction: (SimulationState, SimulationState) => Unit = (_, _) => {}

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

  def withStateChangedFunction(stateChangedFunction: (SimulationState, SimulationState) => Unit): EntitySpawner = {
    this.stateChangedFunction = stateChangedFunction
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
            case None => {
              spawnHelper(FixedEntity(newCircle, infectionTimeSeconds, immuneTimeSeconds,
                stateChangedFunction) :: spawnedFixed, spawnedMovable, startTime)
            }
          }
        } else if (spawnedMovable.length < numberMovableToSpawn) {
          val newMovableCircle = MovableCircle2D.createRandom(outerBoundary, radius, minSpeed, maxSpeed)
          val allCircles: List[CircleShape] = spawnedFixed.map(_.body) ::: spawnedMovable.map(_.body)
          allCircles.find(_.getOverlapWith(newMovableCircle) > 0) match {
            case Some(_) => spawnHelper(spawnedFixed, spawnedMovable, startTime)
            case None => {
              spawnHelper(spawnedFixed,
                MovableEntity(newMovableCircle, infectionTimeSeconds, immuneTimeSeconds,
                  stateChangedFunction) :: spawnedMovable, startTime)
            }
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