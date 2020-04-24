package maak.webapp.simulation

import maak.model.physics2D.OuterBoundary
import maak.model.physics2D.Point2D
import maak.webapp.WebApp._
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{HTMLImageElement, HTMLInputElement}

import scala.scalajs.js

class Simulation(val simulationCanvas: Canvas, val graphCanvas: Canvas, val imageNormal: HTMLImageElement, val imageInfected: HTMLImageElement,
                 val imageImmune: HTMLImageElement) {

  val simulationCtx = simulationCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  val graphCtx = graphCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  var fixedIndividuals: List[FixedIndividual] = List()
  var movableIndividuals: List[MovableIndividual] = List()
  val margin = 20.0
  val outerBoundary = OuterBoundary(Point2D(margin, margin), Point2D(simulationCanvas.width - margin, simulationCanvas.height - margin))

  // Set previous time to current time, this will constantly update in the loop
  var prev = js.Date.now()
  var prevGraphRender = js.Date.now()

  def getImage(individual: Individual): HTMLImageElement = individual.infected match {
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

    val tuple = IndividualSpawner(outerBoundary: OuterBoundary, minSpeed = speed,
      maxSpeed = speed, minRadius = radius, maxRadius = radius, infectionTime = infectionTime, immuneTime = immuneTime,
      maxSpawnTime = 1.0).spawnIndividuals(numberFixed, numberMovable, 1)

    fixedIndividuals = tuple._1
    movableIndividuals = tuple._2

    document.getElementById(FixedIndividualsInfo.labelId).textContent = "Fixed Individuals: " + fixedIndividuals.length
    document.getElementById(MovableIndividualsInfo.labelId).textContent = "Movable Individuals: " + movableIndividuals.length
    document.getElementById(SpeedInfo.labelId).textContent = "Speed: " + speed
    document.getElementById(RadiusInfo.labelId).textContent = "Radius: " + radius
    document.getElementById(InfectionTimeInfo.labelId).textContent = "InfectionTime: " + infectionTime
    document.getElementById(ImmuneTimeInfo.labelId).textContent = "ImmuneTime: " + immuneTime
  }

  // Update game objects
  def update(passedTime: Double) {
    updateAllIndividuals(passedTime, outerBoundary, fixedIndividuals, movableIndividuals)
  }

  def renderSimulation() {
    simulationCtx.clearRect(0, 0, simulationCanvas.width, simulationCanvas.height)

    simulationCtx.strokeStyle = "#2196F3"
    simulationCtx.strokeRect(margin, margin, simulationCanvas.width-margin*2, simulationCanvas.height-margin*2)

    movableIndividuals.map(individual => {
      simulationCtx.drawImage(getImage(individual),
        math.floor(individual.body.position.x - individual.body.radius),
        math.floor(individual.body.position.y - individual.body.radius),
        math.floor(individual.body.radius*2),
        math.floor(individual.body.radius*2))
    })

    fixedIndividuals.map(individual => {
      simulationCtx.drawImage(getImage(individual),
        math.floor(individual.body.position.x - individual.body.radius),
        math.floor(individual.body.position.y - individual.body.radius),
        math.floor(individual.body.radius*2),
        math.floor(individual.body.radius*2))
    })
  }

  var historicValues: List[(Double, Double)] = List()

  def renderGraph() {
    val allIndividuals: List[Individual] = fixedIndividuals ::: movableIndividuals
    val allInfected = allIndividuals.filter(_.infected)
    val allImmune = allIndividuals.filter(_.immune)

    val totalNum = allIndividuals.length.toDouble
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

  val simulationLoop = () => {
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
