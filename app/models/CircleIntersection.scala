package models

import math.Pi
import Scalar.format

case class CircleIntersection(i: IdealCircleIntersection,
                              c1Center: Point,
                              rotation: Double) {

  def chord = (i.chord rotate rotation) + c1Center

  def cross = (i.cross rotate rotation) + c1Center

  def area = i.area

  override def toString = s"$i, rotation = ${format(rotation)} (${180 * rotation / Pi} degrees)"

  def toSvgChord = chord.p1.toSvgPoint(i.c1.label + i.c2.label + "-chord")

  def toSvgP1String = chord.p1.toSvgCoordString

}