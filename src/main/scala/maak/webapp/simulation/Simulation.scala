package maak.webapp.simulation

import com.sun.org.apache.xerces.internal.impl.validation.EntityState
import maak.model.physics2D.shapes.BoundaryBox
import maak.model.physics2D.Position2D
import maak.webapp.WebApp._
import org.scalajs.dom
import org.scalajs.dom.{CanvasRenderingContext2D, document}
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{HTMLImageElement, HTMLInputElement}

import scala.scalajs.js

/** The class representing the simulation */
class Simulation(val simulationCanvas: Canvas, val graphCanvas: Canvas, val imageNormal: HTMLImageElement,
                 val imageInfected: HTMLImageElement,
                 val imageImmune: HTMLImageElement) {

  //Simulation canvas context
  private val simulationCtx = simulationCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  //The margin for the simulation canvas (how far from the edge the entities bounces)
  private val margin = 20.0
  //The boundary based on the margin
  private val outerBoundary: BoundaryBox = BoundaryBox(Position2D(margin, margin),
    Position2D(simulationCanvas.width - margin, simulationCanvas.height - margin))
  //The previous graph simulation rendering time
  private var prevSimulationRender: Double = js.Date.now() / 1000

  //Graph canvas context
  private val graphCtx = graphCanvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  //The graph rendering interval in seconds
  private val graphRenderingInterval = 0.25
  //Historical values for rendering the graph
  private var historicValues: List[(Double, Double)] = List()
  //The previous graph rendering time
  private var prevGraphRender: Double = js.Date.now() / 1000

  //Non moving entities
  private var fixedEntities: List[FixedEntity] = List()
  //Moving entities
  private var movableEntities: List[MovableEntity] = List()

  //Keeping score of how many entities exists in different states
  private var totalNumberOfNormalEntities = 0
  private var totalNumberOfInfectedEntities = 0
  private var totalNumberOfImmuneEntities = 0

  private val entityChangedStateFunction: (SimulationState, SimulationState) => Unit = (_, toState) => toState match {
    case Normal => {
      totalNumberOfNormalEntities += 1
      totalNumberOfImmuneEntities -= 1
    }
    case Infected => {
      totalNumberOfInfectedEntities += 1
      totalNumberOfNormalEntities -= 1
    }
    case Immune => {
      totalNumberOfImmuneEntities += 1
      totalNumberOfInfectedEntities -= 1
    }
  }

  // The simulation loop function that the browser calls at a given interval
  private val simulationLoop: () => Unit = () => {
    val now = js.Date.now() / 1000
    val timeDeltaInSeconds = now - prevSimulationRender

    //Update all entities
    updateAllEntities(timeDeltaInSeconds, outerBoundary, fixedEntities, movableEntities)
    //Render the simulation canvas
    renderSimulation()
    //Render the graph if its time
    if (now - prevGraphRender > graphRenderingInterval) {
      renderGraph()
      prevGraphRender = now
    }

    prevSimulationRender = now
  }

  /** resets the simulation (called every time a slider is changed) */
  def resetSimulation(): Unit = {
    //Prints to the developer console to indicate a reset has occurred
    println("Reset")

    //Clear historical values and clean the graph canvas
    historicValues = List()
    graphCtx.clearRect(0, 0, graphCanvas.width, graphCanvas.height)

    //Get all new values from the sliders
    val numberFixed = document.getElementById(
      FixedEntitiesInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber.toInt
    val numberMovable = document.getElementById(
      MovableEntitiesInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber.toInt
    val maxSpeed = document.getElementById(MaxSpeedInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber
    val radius = document.getElementById(RadiusInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber
    val infectionTime = document.getElementById(InfectionTimeInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber
    val immuneTime = document.getElementById(ImmuneTimeInfo.sliderId).asInstanceOf[HTMLInputElement].valueAsNumber

    //Spawn new entities based on the values (note - all entities might not be able to spawn)
    val entitiesTuple = new EntitySpawner().withOuterBoundary(outerBoundary).
      withInfectionTimeSeconds(infectionTime).withImmuneTimeSeconds(immuneTime).
      withMinSpeed(maxSpeed / 2).withMaxSpeed(maxSpeed).withRadius(radius).withNumberFixedToSpawn(numberFixed).
      withNumberMovableToSpawn(numberMovable).withNumberMovableInfected(1).withMaxSpawnTimeInSeconds(10).
      withStateChangedFunction(entityChangedStateFunction).spawn()

    //Put the entities in its respective list
    fixedEntities = entitiesTuple._1
    movableEntities = entitiesTuple._2

    //Change values of labels to correctly correspond to the new values
    document.getElementById(FixedEntitiesInfo.labelId).textContent = "Fixed entities: " + fixedEntities.length
    document.getElementById(MovableEntitiesInfo.labelId).textContent = "Movable entities: " + movableEntities.length
    document.getElementById(MaxSpeedInfo.labelId).textContent = "Max speed: " + maxSpeed
    document.getElementById(RadiusInfo.labelId).textContent = "Radius: " + radius
    document.getElementById(InfectionTimeInfo.labelId).textContent = "InfectionTime: " + infectionTime
    document.getElementById(ImmuneTimeInfo.labelId).textContent = "ImmuneTime: " + immuneTime

    //Make sure the numbers of entities exists in different states are correct
    totalNumberOfNormalEntities = (fixedEntities.filter(_.state == Normal) ::: movableEntities.
      filter(_.state == Normal)).length
    totalNumberOfInfectedEntities = (fixedEntities.filter(_.state == Infected) ::: movableEntities.
      filter(_.state == Infected)).length
    totalNumberOfImmuneEntities = (fixedEntities.filter(_.state == Immune) ::: movableEntities.
      filter(_.state == Immune)).length
  }

  // Renders the simulation canvas
  private def renderSimulation(): Unit = {
    //Clears the whole canvas
    simulationCtx.clearRect(0, 0, simulationCanvas.width, simulationCanvas.height)

    //Draw the boundary box
    simulationCtx.strokeStyle = "#2196F3"
    simulationCtx.strokeRect(margin, margin, simulationCanvas.width - margin * 2, simulationCanvas.height - margin * 2)

    //Draw all fixed entities
    fixedEntities.foreach(fixedEntity => {
      simulationCtx.drawImage(getImage(fixedEntity.state),
        math.floor(fixedEntity.body.center.x - fixedEntity.body.radius),
        math.floor(fixedEntity.body.center.y - fixedEntity.body.radius),
        math.floor(fixedEntity.body.diameter),
        math.floor(fixedEntity.body.diameter))
    })

    //Draw all moving entities
    movableEntities.foreach(movableEntity => {
      simulationCtx.drawImage(getImage(movableEntity.state),
        math.floor(movableEntity.body.center.x - movableEntity.body.radius),
        math.floor(movableEntity.body.center.y - movableEntity.body.radius),
        math.floor(movableEntity.body.diameter),
        math.floor(movableEntity.body.diameter))
    })
  }

  // Returns the correct image to draw depending on entity state
  private def getImage(state: SimulationState): HTMLImageElement = state match {
    case Infected => imageInfected
    case Immune => imageImmune
    case Normal => imageNormal
  }

  // Render the graph canvas
  private def renderGraph(): Unit = {
    //Update the labels that shows the current number of entities in each state
    document.getElementById(HealthyEntitiesLabel.labelId).textContent = totalNumberOfNormalEntities.toString
    document.getElementById(InfectedEntitiesLabel.labelId).textContent = totalNumberOfInfectedEntities.toString
    document.getElementById(ImmuneEntitiesLabel.labelId).textContent = totalNumberOfImmuneEntities.toString

    //Calculate the difference to get a percentage
    val totalNum = (totalNumberOfNormalEntities + totalNumberOfInfectedEntities + totalNumberOfImmuneEntities).toDouble
    val immunePart = totalNumberOfImmuneEntities.toDouble / totalNum
    val infectedPart = totalNumberOfInfectedEntities.toDouble / totalNum

    //Add the new values to the list of historical values
    historicValues = historicValues ::: List((math.floor(graphCanvas.height * immunePart),
      math.floor(graphCanvas.height * infectedPart)))

    //If the graph doesn't fit more values, start removing old ones
    if (historicValues.length > graphCanvas.width) {
      historicValues = historicValues.tail
    }

    //Loop through all historical values and draw the graph
    for ((x, (immuneHeight, infectedHeight)) <- historicValues.indices.zip(historicValues)) {
      graphCtx.fillStyle = "#4CAF50"
      graphCtx.fillRect(x, 0, 1.0, immuneHeight)

      val normalHeight = graphCanvas.height - (immuneHeight + infectedHeight)

      graphCtx.fillStyle = "#2196F3"
      graphCtx.fillRect(x, immuneHeight, 1.0, normalHeight)

      graphCtx.fillStyle = "#f44336"
      graphCtx.fillRect(x, immuneHeight + normalHeight, 1.0, infectedHeight)
    }
  }

  //Set the simulation loop to run at a specific interval
  dom.window.setInterval(simulationLoop, 10)
}
