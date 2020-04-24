package maak.webapp

import maak.webapp.simulation.Simulation
import HtmlHelper._
import maak.webapp.HtmlHelper.MyCss.MyCssSlider
import maak.webapp.HtmlHelper.W3Css.{W3CssBlue, W3CssBorder, W3CssCenter, W3CssContainer, W3CssGreen, W3CssPanel, W3CssRed, W3CssRoundxxLarge, W3CssRow, W3CssRowPadding, W3CssSection, W3CssStretch, W3colThird, W3colTwoThird}
import org.scalajs.dom
import org.scalajs.dom.document
import org.scalajs.dom.html.Canvas
import org.scalajs.dom.raw.{HTMLImageElement, HTMLInputElement, Node}

object WebApp {

  sealed abstract class SliderLabelInfo(val sliderId: String, val labelId: String, val min: Int, val max: Int, val defaultValue: Int)
  case object FixedIndividualsInfo extends SliderLabelInfo("fixedIndividualsSlider", "fixedIndividualsLabel", 1, 200, 20)
  case object MovableIndividualsInfo extends SliderLabelInfo("movableIndividualsSlider", "movableIndividualsLabel",1, 200, 50)
  case object SpeedInfo extends SliderLabelInfo("speedSlider", "speedLabel", 1, 500, 100)
  case object RadiusInfo extends SliderLabelInfo("radiusSlider", "radiusLabel", 4, 20, 8)
  case object InfectionTimeInfo extends SliderLabelInfo("infectionTimeSlider", "infectionTimeLabel", 1, 30, 8)
  case object ImmuneTimeInfo extends SliderLabelInfo("immuneTimeSlider", "immuneTimeLabel", 1, 30, 8)

  sealed abstract class LabelInfo(val labelId: String)
  case object HealthyIndividualsLabel extends LabelInfo("healthyIndividualsLabel")
  case object InfectedIndividualsLabel extends LabelInfo("infectedIndividualsLabel")
  case object ImmuneIndividualsLabel extends LabelInfo("immuneIndividualsLabel")

  def createSliderLabelBox(parent: Node, sliderLabelInfo: SliderLabelInfo, onchangeFunc: dom.Event => Unit): HTMLInputElement = {
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

  def main(args: Array[String]): Unit = {
    document.addEventListener("DOMContentLoaded", { _: dom.Event =>
      setupUI()
    })
  }

  def setupUI(): Unit = {
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

    createSliderLabelBox(rightColumn, FixedIndividualsInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, MovableIndividualsInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, SpeedInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, RadiusInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, InfectionTimeInfo, (_: dom.Event) => simulation.resetSimulation())
    createSliderLabelBox(rightColumn, ImmuneTimeInfo, (_: dom.Event) => simulation.resetSimulation())

    val labelRow = createDiv(List(W3CssRowPadding, W3CssStretch))
    rightColumn.appendChild(labelRow)

    val healthyCol = createDiv(List(W3colThird, W3CssContainer))
    val healthyIndividuals = createDiv(List(W3CssPanel, W3CssRoundxxLarge, W3CssBlue, W3CssCenter))
    healthyIndividuals.id = HealthyIndividualsLabel.labelId
    healthyCol.appendChild(healthyIndividuals)
    labelRow.appendChild(healthyCol)

    val infectedCol = createDiv(List(W3colThird, W3CssContainer))
    val infectedIndividuals = createDiv(List(W3CssPanel, W3CssRoundxxLarge, W3CssRed, W3CssCenter))
    infectedIndividuals.id = InfectedIndividualsLabel.labelId
    infectedCol.appendChild(infectedIndividuals)
    labelRow.appendChild(infectedCol)

    val immuneCol = createDiv(List(W3colThird, W3CssContainer))
    val immuneIndividuals = createDiv(List(W3CssPanel, W3CssRoundxxLarge, W3CssGreen, W3CssCenter))
    immuneIndividuals.id = ImmuneIndividualsLabel.labelId
    immuneCol.appendChild(immuneIndividuals)
    labelRow.appendChild(immuneCol)

    simulation.resetSimulation()
  }
}