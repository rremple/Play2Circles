package models

import math.{min,max}

case class GridDimension(cs: List[Circle]) {

  val minGrid = Point(
    cs.map(c => c.center.x - c.radius).min - 1,
    cs.map(c => c.center.y - c.radius).min - 1)

  val maxGrid = Point(
    cs.map(c => c.center.x + c.radius).max + 1,
    cs.map(c => c.center.y + c.radius).max + 1)

  val rangeGridX = maxGrid.x - minGrid.x

  val rangeGridY = maxGrid.y - minGrid.y

  val middleGridX = (maxGrid.x + minGrid.x) / 2

  val middleGridY = (maxGrid.y + minGrid.y) / 2

  override def toString = s"range = $minGrid to $maxGrid"

  def toSvgAxes =
    <g>
      <line id="x-axis" x1={ minGrid.x.toString } y1="0" x2={ maxGrid.x.toString } y2="0"/>
      <line id="y-axis" x1="0" y1={ minGrid.y.toString } x2="0" y2={ maxGrid.y.toString }/>
      <rect id="box" x={ minGrid.x.toString } y={ minGrid.y.toString } width="100%" height="100%"/>
    </g>

  def toSvgGrid =
    <g>
      <line id="x-grid" stroke-width={ rangeGridX.toString } x1={ middleGridX.toString } y1={ (math.ceil(minGrid.y) - 0.05).toString } x2={ middleGridX.toString } y2={ maxGrid.y.toString }/>
      <line id="y-grid" stroke-width={ rangeGridY.toString } x1={ (math.ceil(minGrid.x) - 0.05).toString } y1={ middleGridY.toString } x2={ maxGrid.x.toString } y2={ middleGridY.toString }/>
    </g>

  def toSvgViewBoxAttribute =
    minGrid.x.toString + " " + minGrid.y.toString + " " + rangeGridX.toString + " " + rangeGridY.toString
}

object GridDimension {
  def apply(c: Circle*) = new GridDimension(c.toList)
}