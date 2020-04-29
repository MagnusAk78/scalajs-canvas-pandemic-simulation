package maak.webapp

import maak.webapp.simulation.Simulation
import HtmlHelper._
import maak.webapp.HtmlHelper.MyCss.MyCssSlider
import maak.webapp.HtmlHelper.W3Css._
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{HTMLImageElement, HTMLInputElement, Node}

/** The web application */
object WebApp {
  sealed abstract class SliderLabelInfo(val sliderId: String, val labelId: String, val min: Int, val max: Int,
                                        val defaultValue: Int)
  case object FixedEntitiesInfo extends SliderLabelInfo("fixedEntitiesSlider", "fixedEntitiesLabel", 1, 200, 20)
  case object MovableEntitiesInfo extends SliderLabelInfo("movableEntitiesSlider", "movableEntitiesLabel",1, 200, 50)
  case object MaxSpeedInfo extends SliderLabelInfo("maxSpeedSlider", "maxSpeedLabel", 1, 500, 100)
  case object RadiusInfo extends SliderLabelInfo("radiusSlider", "radiusLabel", 4, 20, 8)
  case object InfectionTimeInfo extends SliderLabelInfo("infectionTimeSlider", "infectionTimeLabel", 1, 30, 8)
  case object ImmuneTimeInfo extends SliderLabelInfo("immuneTimeSlider", "immuneTimeLabel", 1, 30, 8)

  sealed abstract class LabelInfo(val labelId: String)
  case object HealthyEntitiesLabel extends LabelInfo("healthyEntitiesLabel")
  case object InfectedEntitiesLabel extends LabelInfo("infectedEntitiesLabel")
  case object ImmuneEntitiesLabel extends LabelInfo("immuneEntitiesLabel")

  //Helper method that creates a div node with a slider and label
  private def createSliderLabelBox(parent: Node, sliderLabelInfo: SliderLabelInfo,
                                   onchangeFunc: dom.Event => Unit): HTMLInputElement = {
    val sliderSection = createDiv(List(W3CssSection))
    val sliderRow = createDiv(List(W3CssRow))
    val labelRow = createDiv(List(W3CssRow))

    val inputSlider = dom.document.createElement("input").asInstanceOf[HTMLInputElement]
    inputSlider.id = sliderLabelInfo.sliderId
    inputSlider.`type` = "range"
    inputSlider.min = sliderLabelInfo.min.toString
    inputSlider.max = sliderLabelInfo.max.toString
    inputSlider.value = sliderLabelInfo.defaultValue.toString
    inputSlider.className = MyCssSlider.className

    inputSlider.onchange = onchangeFunc

    sliderRow.appendChild(inputSlider)

    val label = dom.document.createElement("label")
    label.id = sliderLabelInfo.labelId
    labelRow.appendChild(label)

    sliderSection.appendChild(sliderRow)
    sliderSection.appendChild(labelRow)
    parent.appendChild(sliderSection)

    inputSlider
  }

  /** main method, entry point of the application */
  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { _: dom.Event =>
      setupUI()
    })
  }

  //Setups the web UI and creates a simulation
  private def setupUI(): Unit = {
    val row = createDiv(List(W3CssRowPadding))
    val leftColumn = createDiv(List(W3colTwoThird, W3CssContainer))
    val rightColumn = createDiv(List(W3colThird, W3CssContainer))
    row.appendChild(leftColumn)
    row.appendChild(rightColumn)
    dom.document.body.appendChild(row)

    val simulationCanvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    simulationCanvas.width = leftColumn.clientWidth
    simulationCanvas.height = (0.95 * dom.window.innerHeight).toInt
    simulationCanvas.style = "background: black; margin: 0; padding: 0; display: block; width: 100%; height: 100%;}"
    leftColumn.appendChild(simulationCanvas)

    val graphCanvas = dom.document.createElement("canvas").asInstanceOf[Canvas]
    graphCanvas.width = rightColumn.clientWidth
    graphCanvas.height = (0.25 * dom.window.innerHeight).toInt
    graphCanvas.style = "background: black; margin: 0; padding: 0; display: block; width: 100%; height: 100%;"
    rightColumn.appendChild(graphCanvas)

    val sphereGrey = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    val sphereRed = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    val sphereGreen = dom.document.createElement("img").asInstanceOf[HTMLImageElement]
    sphereGrey.src = "images/sphere-grey.png"
    sphereRed.src = "images/sphere-red.png"
    sphereGreen.src = "images/sphere-green.png"
    val simulation = new Simulation(simulationCanvas, graphCanvas, sphereGrey, sphereRed, sphereGreen)

    createSliderLabelBox(rightColumn, FixedEntitiesInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, MovableEntitiesInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, MaxSpeedInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, RadiusInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, InfectionTimeInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, ImmuneTimeInfo, (_: dom.Event) => simulation.resetSimulation())

    val labelRow = createDiv(List(W3CssRowPadding, W3CssStretch))
    rightColumn.appendChild(labelRow)

    val healthyCol = createDiv(List(W3colThird, W3CssContainer))
    val healthyEntities = createDiv(List(W3CssPanel, W3CssRoundxxLarge, W3CssBlue, W3CssCenter))
    healthyEntities.id = HealthyEntitiesLabel.labelId
    healthyCol.appendChild(healthyEntities)
    labelRow.appendChild(healthyCol)

    val infectedCol = createDiv(List(W3colThird, W3CssContainer))
    val infectedEntities = createDiv(List(W3CssPanel, W3CssRoundxxLarge, W3CssRed, W3CssCenter))
    infectedEntities.id = InfectedEntitiesLabel.labelId
    infectedCol.appendChild(infectedEntities)
    labelRow.appendChild(infectedCol)

    val immuneCol = createDiv(List(W3colThird, W3CssContainer))
    val immuneEntities = createDiv(List(W3CssPanel, W3CssRoundxxLarge, W3CssGreen, W3CssCenter))
    immuneEntities.id = ImmuneEntitiesLabel.labelId
    immuneCol.appendChild(immuneEntities)
    labelRow.appendChild(immuneCol)

    simulation.resetSimulation()
  }
}