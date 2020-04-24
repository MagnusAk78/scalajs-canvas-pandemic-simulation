package maak.webapp

import org.scalajs.dom
import org.scalajs.dom.raw.{HTMLDivElement, HTMLInputElement, Node}

//Convenient object for W3CSS (https://www.w3schools.com/w3css/default.asp)
object HtmlHelper {
  abstract class ClassName(val className: String)

  def createDiv(w3CssClassNames: List[ClassName] = List()): HTMLDivElement = {
    val divElement = dom.document.createElement("div").asInstanceOf[HTMLDivElement]
    divElement.className = w3CssClassNames.map(_.className).fold("")((s1: String, s2: String) => s1 + " " + s2)
    divElement
  }

  object MyCss {
    case object MyCssSlider extends ClassName("slider")
  }

  object W3Css {
    //Common
    case object W3CssContainer extends ClassName("w3-container")
    case object W3CssStretch extends ClassName("w3-stretch")
    case object W3CssSection extends ClassName("w3-section")
    case object W3CssPanel extends ClassName("w3-panel")

    //Alignment
    case object W3CssCenter extends ClassName("w3-center")

    //Responsive rows and columns
    case object W3CssRow extends ClassName("w3-row")
    case object W3CssRowPadding extends ClassName("w3-row-padding")
    case object W3colHalf extends ClassName("w3-half")
    case object W3colThird extends ClassName("w3-third")
    case object W3colTwoThird extends ClassName("w3-twothird")
    case object W3colQuarter extends ClassName("w3-quarter")
    case object W3colThreeQuarter extends ClassName("w3-threequarter")
    case object W3colRest extends ClassName("w3-rest")
    case object W3colCol extends ClassName("w3-col")

    //Borders
    case object W3CssBorder extends ClassName("w3-border")

    //Round
    case object W3CssRound extends ClassName("w3-round")
    case object W3CssRoundxxLarge extends ClassName("w3-round-xxlarge")

    //Colors
    case object W3CssBlue extends ClassName("w3-blue")
    case object W3CssRed extends ClassName("w3-red")
    case object W3CssGreen extends ClassName("w3-green")
  }
}
