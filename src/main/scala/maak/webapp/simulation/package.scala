package maak.webapp

import maak.model.physics2D.Collisions._
import maak.model.physics2D._

import scala.annotation.tailrec
import scala.scalajs.js

package object simulation {

  sealed abstract class Individual(val body: Circle, val infectionTime: Double, val immuneTime: Double) {
    var infected: Boolean = false
    var immune: Boolean = false
    var timeInfected: Double = 0.0
    var timeImmune: Double = 0.0

    def infect() {
      if(!infected && !immune) {
        infected = true
        timeInfected = js.Date.now()
      }
    }

    def update() {
      if(infected && (js.Date.now() - timeInfected)/1000 > infectionTime) {
        infected = false
        immune = true
        timeImmune = js.Date.now()
      }

      if(immune && (js.Date.now() - timeImmune)/1000 > immuneTime) {
        immune = false
      }
    }
  }
  case class MovableIndividual(override val body: MovableCircle, override val infectionTime: Double,
                               override val immuneTime: Double) extends Individual(body, infectionTime, immuneTime)
  case class FixedIndividual(override val body: FixedCircle, override val infectionTime: Double,
                             override val immuneTime: Double) extends Individual(body, infectionTime, immuneTime)

  case class IndividualSpawner(outerBoundary: Rectangle2D, minSpeed: Double = 10.0, maxSpeed: Double = 10.0,
                               minRadius: Double = 10.0, maxRadius: Double = 10.0, infectionTime: Double,
                               immuneTime: Double, maxSpawnTime: Double = 1.0) {
    val circleSpawner = CircleSpawner(outerBoundary, minSpeed, maxSpeed, minRadius, maxRadius, maxSpawnTime)

    def spawnIndividuals(numFixed: Int, numMovable: Int, numInfected: Int): (List[FixedIndividual], List[MovableIndividual]) = {
      var fixedIndividuals: List[FixedIndividual] = List()
      var movableIndividuals: List[MovableIndividual] = List()
      for(i <- 1 to numFixed) {
        fixedIndividuals = circleSpawner.spawnRandomFixedCircle(fixedIndividuals.map(_.body)) match {
          case Some(fixedCircle: FixedCircle) => FixedIndividual(fixedCircle, infectionTime, immuneTime) :: fixedIndividuals
          case None => fixedIndividuals
        }
      }
      for(i <- 1 to numMovable) {
        movableIndividuals = circleSpawner.spawnRandomMovableCircle(movableIndividuals.map(_.body) ::: fixedIndividuals.map(_.body)) match {
          case Some(movableCircle: MovableCircle) => {
            val newMovableIndividual = MovableIndividual(movableCircle, infectionTime, immuneTime)
            if(movableIndividuals.size < numInfected)
              newMovableIndividual.infect()
            newMovableIndividual :: movableIndividuals
          }
          case None => movableIndividuals
        }
      }
      (fixedIndividuals, movableIndividuals)
    }
  }

  // Update
  def updateAllIndividuals(passedTime: Double, boundary: Rectangle2D, fixedIndividuals: List[FixedIndividual],
                           movableIndividuals: List[MovableIndividual]) {

    movableIndividuals.foreach(_.body.move(passedTime))

    @tailrec
    def updateAllIndividualsInternal(passedTime: Double, boundary: Rectangle2D,
                                     fixedIndividuals: List[FixedIndividual],
                                     movableIndividuals: List[MovableIndividual]) {
      movableIndividuals match {
        case movableIndividual :: restMovableIndividuals => {
          for(otherMovableIndividual <- restMovableIndividuals) {
            if(checkForCollisionAndUpdate(movableIndividual.body, otherMovableIndividual.body)) {
              (movableIndividual.infected, otherMovableIndividual.infected) match {
                case (true, false) => otherMovableIndividual.infect()
                case (false, true) => movableIndividual.infect()
                case _ =>
              }
            }
          }
          for(otherFixedIndividual <- fixedIndividuals) {
            if(checkForCollisionAndUpdate(movableIndividual.body, otherFixedIndividual.body)) {
              (movableIndividual.infected, otherFixedIndividual.infected) match {
                case (true, false) => otherFixedIndividual.infect()
                case (false, true) => movableIndividual.infect()
                case _ =>
              }
            }
          }
          checkForCollisionAndUpdate(movableIndividual.body, boundary)
          updateAllIndividualsInternal(passedTime, boundary, fixedIndividuals, restMovableIndividuals)
        }
        case Nil => // Empty List
      }
    }
    updateAllIndividualsInternal(passedTime, boundary, fixedIndividuals, movableIndividuals)
    fixedIndividuals.foreach(_.update())
    movableIndividuals.foreach(_.update())
  }
}
