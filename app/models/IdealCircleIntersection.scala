package models

import math.{ Pi, sqrt, pow, abs, min, max, cos, sin, acos }

case class IdealCircleIntersection(c1: IdealCircle, c2: IdealCircle, interesctionArea: Double) {

  private def square(x: Double) = pow(x, 2)

  val r1 = c1.radius
  val r2 = c2.radius

  if ((interesctionArea > c1.area) || (interesctionArea > c2.area))
    throw new RuntimeException(c1.label + "\u2229" + c2.label + " is too large.")

  /*
   * returns the area and the chord at sep as a pair
   */
  def intersectionAttributes(guessSep: Double) = {
    val chordX = (square(r1) - square(r2) + square(guessSep)) / (2 * guessSep)
    val chordY = sqrt(square(r1) - square(chordX))
    val sector1Area = square(r1) * acos(chordX / r1)
    val sector2Area = square(r2) * acos((guessSep - chordX) / r2)
    val sectorIntersectionArea = chordY * guessSep
    (sector1Area + sector2Area - sectorIntersectionArea,
      LineSegment(Point(chordX, chordY), Point(chordX, -chordY)))
  }

  val sep = {
    if ((r1 == 0) || (r2 == 0)) 0
    else Solver.solveForDecreasing("intersecting circle separation", abs(r1 - r2), r1 + r2, interesctionArea) {
      intersectionAttributes(_)._1
    }
  }

  def area = intersectionAttributes(sep)._1

  def chord = intersectionAttributes(sep)._2

  def cross = LineSegment(Point(sep - c2.radius, 0), Point(c1.radius, 0))

  override def toString = s"$c1.label \u2229 $c2.label, separation = $sep"

  def realizeWith(c1Center: Point, rotation: Double) = CircleIntersection(this, c1Center, rotation)
}