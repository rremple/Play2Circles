package models

import math.{ Pi, sqrt, pow, abs, min, max, cos, sin, acos }

case class IdealCircle(label: String, area: Double) {

  val radius = sqrt(area / Pi)

  override def toString = s"Circle $label: radius=$radius"

  private def square(x: Double) = pow(x, 2)

  def realizeWith(center: Point) = Circle(this, center)

  private def chordAreaByCoord(chordX: Double, chordY: Double) =
    square(radius) * acos(chordX / radius) - chordY * chordX

  /*
   * returns the area defined by a chord at this distance from the center (-radius <= distanceX <= radius)
   * and the chord itself as a pair
   */
  def chordAttributes(chordX: Double) = {
    val chordY = sqrt(square(radius) - square(chordX))
    (chordAreaByCoord(chordX, chordY), LineSegment(Point(chordX, chordY), Point(chordX, -chordY)))
  }

  /*
   * returns the area defined by the line segment as a chord 
   * and returns the distance from the center to this chord
   */
  def chordAttributes(seg: LineSegment) = {
    val chordY = seg.length / 2
    val chordX = sqrt(square(radius) - square(chordY))
    (chordAreaByCoord(chordX, chordY), chordX)
  }

  def chord(areaTarget: Double) = {
    val chordX = Solver.solveForDecreasing("area defined by chord", -radius, radius, areaTarget) {
      chordAttributes(_)._1
    }
    val chordY = sqrt(square(radius) - square(chordX))
    LineSegment(Point(chordX, chordY), Point(chordX, -chordY))
  }

}