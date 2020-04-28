package maak.webapp.simulation

import maak.model.physics2D.shapes.BoundaryBox
import maak.model.physics2D.Position2D
import maak.webapp.WebApp._
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, document}
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{HTMLImageElement, HTMLInputElement}

import scala.scalajs.js

class Simulation(val simulationCanvas: Canvas, val graphCanvas: Canvas, val imageNormal: HTMLImageElement, val imageInfected: HTMLImageElement,
                 val imageImmune: HTMLImageElement) {

  private val simulationCtx = simulationCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private val graphCtx = graphCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  private var fixedEntities: List[FixedEntity] = List()
  private var movableEntities: List[MovableEntity] = List()
  private val margin = 20.0
  private val outerBoundary: BoundaryBox = BoundaryBox(Position2D(margin, margin),
    Position2D(simulationCanvas.width - margin, simulationCanvas.height - margin))

  // Set previous time to current time, this will constantly update in the loop
  private var prev: Double = js.Date.now()
  private var prevGraphRender: Double = js.Date.now()

  private def getImage(individual: Entity): HTMLImageElement = individual.infected match {
    case true => imageInfected
    case false => individual.immune match {
      case true => imageImmune
      case false => imageNormal
    }
  }

  def resetSimulation(): Unit = {

    println("Reset")

    historicValues = List()
    graphCtx.clearRect(0, 0, graphCanvas.width, graphCanvas.height)

    val numberFixed = document.getElementById(FixedIndividualsInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber.toInt
    val numberMovable = document.getElementById(MovableIndividualsInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber.toInt
    val speed = document.getElementById(SpeedInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber
    val radius = document.getElementById(RadiusInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber
    val infectionTime = document.getElementById(InfectionTimeInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber
    val immuneTime = document.getElementById(ImmuneTimeInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber

    val entitiesTuple = new EntitySpawner().withOuterBoundary(outerBoundary).
      withInfectionTimeSeconds(infectionTime).withImmuneTimeSeconds(immuneTime).
      withMinSpeed(speed/2).withMaxSpeed(speed).withRadius(radius).withNumberFixedToSpawn(numberFixed).
      withNumberMovableToSpawn(numberMovable).withNumberMovableInfected(1).withMaxSpawnTimeInSeconds(10).spawn()

    fixedEntities = entitiesTuple._1
    movableEntities = entitiesTuple._2

    document.getElementById(FixedIndividualsInfo.labelId).textContent = "Fixed Individuals: " + fixedEntities.length
    document.getElementById(MovableIndividualsInfo.labelId).textContent = "Movable Individuals: " + movableEntities.length
    document.getElementById(SpeedInfo.labelId).textContent = "Speed: " + speed
    document.getElementById(RadiusInfo.labelId).textContent = "Radius: " + radius
    document.getElementById(InfectionTimeInfo.labelId).textContent = "InfectionTime: " + infectionTime
    document.getElementById(ImmuneTimeInfo.labelId).textContent = "ImmuneTime: " + immuneTime
  }

  // Update game objects
  private def update(passedTime: Double) {
    updateAllEntities(passedTime, outerBoundary, fixedEntities, movableEntities)
  }

  private def renderSimulation() {
    simulationCtx.clearRect(0, 0, simulationCanvas.width, simulationCanvas.height)

    simulationCtx.strokeStyle = "#2196F3"
    simulationCtx.strokeRect(margin, margin, simulationCanvas.width-margin*2, simulationCanvas.height-margin*2)

    fixedEntities.foreach(e => {
      simulationCtx.drawImage(getImage(e),
        math.floor(e.body.center.x - e.body.radius),
        math.floor(e.body.center.y - e.body.radius),
        math.floor(e.body.diameter),
        math.floor(e.body.diameter))
    })

    movableEntities.foreach(e => {
      simulationCtx.drawImage(getImage(e),
        math.floor(e.body.center.x - e.body.radius),
        math.floor(e.body.center.y - e.body.radius),
        math.floor(e.body.diameter),
        math.floor(e.body.diameter))
    })
  }

  private var historicValues: List[(Double, Double)] = List()

  private def renderGraph() {
    val allInfected = fixedEntities.filter(_.infected) ::: movableEntities.filter(_.infected)
    val allImmune = fixedEntities.filter(_.immune) ::: movableEntities.filter(_.immune)

    val totalNum = (fixedEntities.length + movableEntities.length).toDouble
    val immuneNum = allImmune.length.toDouble
    val infectedNum = allInfected.length.toDouble
    val healthyNum = totalNum - (immuneNum + infectedNum)

    document.getElementById(HealthyIndividualsLabel.labelId).textContent = healthyNum.toInt.toString
    document.getElementById(InfectedIndividualsLabel.labelId).textContent = infectedNum.toInt.toString
    document.getElementById(ImmuneIndividualsLabel.labelId).textContent = immuneNum.toInt.toString

    val immunePart = immuneNum / totalNum
    val infectedPart = infectedNum / totalNum

    historicValues = historicValues ::: List((math.floor(graphCanvas.height * immunePart),
      math.floor(graphCanvas.height * infectedPart)))

    if(historicValues.length > graphCanvas.width) {
      historicValues = historicValues.tail
    }

    var x = 0.0
    for((immuneHeight, infectedHeight) <- historicValues) {
      graphCtx.fillStyle = "#4CAF50"
      graphCtx.fillRect(x, 0, 1.0, immuneHeight)

      val normalHeight = graphCanvas.height - (immuneHeight + infectedHeight)

      graphCtx.fillStyle = "#2196F3"
      graphCtx.fillRect(x, immuneHeight, 1.0, normalHeight)

      graphCtx.fillStyle = "#f44336"
      graphCtx.fillRect(x, immuneHeight + normalHeight, 1.0, infectedHeight)

      x += 1
    }

    prevGraphRender = js.Date.now()
  }

  private val simulationLoop: () => Unit = () => {
    val now = js.Date.now()
    val timeDeltaInSeconds = (now - prev) / 1000

    update(timeDeltaInSeconds)
    renderSimulation()
    if((now - prevGraphRender) / 1000 > 0.25) {
      renderGraph()
    }

    prev = now
  }

  dom.window.setInterval(simulationLoop, 10)
}
