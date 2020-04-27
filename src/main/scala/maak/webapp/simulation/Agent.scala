package maak.webapp.simulation

import maak.model.physics2D.Vector2D
import maak.model.physics2D.shapes.{Circle2D, CircleShape, Rectangle2D}
import maak.model.physics2D.shapes.collisions.MovableCircle2D

import scala.annotation.tailrec
import scala.scalajs.js
import scala.util.Random

class Agent(var body: CircleShape, val infectionTime: Double, val immuneTime: Double) {
  var infected: Boolean = false
  var immune: Boolean = false
  var timeInfected: Double = 0.0
  var timeImmune: Double = 0.0

  def setVelocity(newVelocity: Vector2D) {
    body match {
      case c: Circle2D => body = MovableCircle2D(c.center, c.radius, newVelocity)
      case mc: MovableCircle2D => body = mc.copy(velocity = newVelocity)
    }
  }

  def stop() {
    body match {
      case _: Circle2D => // Do nothing
      case mc: MovableCircle2D => body = Circle2D(mc.center, mc.radius)
    }
  }

  def infect() {
    if (!infected && !immune) {
      infected = true
      timeInfected = js.Date.now()
    }
  }

  def update() {
    if (infected && (js.Date.now() - timeInfected) / 1000 > infectionTime) {
      infected = false
      immune = true
      timeImmune = js.Date.now()
    }

    if (immune && (js.Date.now() - timeImmune) / 1000 > immuneTime) {
      immune = false
    }
  }
}

object Agent {

  @tailrec
  private def spawnRandomCirclesWithoutCollision(outerBoundary: Rectangle2D, existingCircles: List[Circle2D],
                                                 radius: Double, numToSpawn: Int, spawned: List[Circle2D],
                                                 maxSpawnTimeInSeconds: Double = 1.0): List[Circle2D] = {
    val startTime = js.Date.now()
    val newCircle = Circle2D.createRandom(outerBoundary, radius)

    val allCircles = existingCircles ::: spawned

    allCircles.find(_.getOverlapWith(newCircle) > 0) match {
      case Some(_) => {
        val secondsLeft = maxSpawnTimeInSeconds - ((js.Date.now() - startTime) / 1000)
        if (secondsLeft > 0 && numToSpawn > 0) {
          spawnRandomCirclesWithoutCollision(outerBoundary, existingCircles, radius, numToSpawn,
            spawned, secondsLeft)
        } else {
          spawned
        }
      }
      case None => spawnRandomCirclesWithoutCollision(outerBoundary, existingCircles, radius, numToSpawn - 1,
        newCircle :: spawned, maxSpawnTimeInSeconds)
    }
  }

  def spawnRandomlyWithoutCollision(outerBoundary: Rectangle2D, radius: Double,
                                    maxVelocity: Double, infectionTime: Double,
                                    immuneTime: Double, maxSpawnTimeInSeconds: Double = 5.0,
                                    numFixed: Int, numMoving: Int, numInfected: Int): List[Agent] = {

    val fixedCircles = spawnRandomCirclesWithoutCollision(outerBoundary, List(), radius, numFixed, List(),
      maxSpawnTimeInSeconds)

    val movingCircles = spawnRandomCirclesWithoutCollision(outerBoundary, fixedCircles, radius, numMoving, List(),
      maxSpawnTimeInSeconds)

    val fixedAgents = fixedCircles.map((c: Circle2D) => new Agent(body = c, infectionTime, immuneTime))

    val movingAgents = movingCircles.map((c: Circle2D) => new Agent(body = c, infectionTime, immuneTime))

    movingAgents.foreach(_.setVelocity(Vector2D.createRandomUnit.scaleTo(Random.between(maxVelocity/2, maxVelocity))))

    movingAgents.take(math.min(numInfected, movingAgents.length)).foreach(_.infect())

    movingAgents ::: fixedAgents
  }
}