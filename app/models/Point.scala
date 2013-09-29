package models

import math.{ cos, sin }

case class Point(x: Double, y: Double) {

  private def square(n: Double) = n * n

  def distanceTo(p: Point) = math.sqrt(square(x - p.x) + square(y - p.y))

  def closeTo(p: Point) = Scalar.eq(distanceTo(p), 0)

  def +(p: Point) = Point(x + p.x, y + p.y)

  def mid(p: Point) = Point((x + p.x) / 2, (y + p.y) / 2)

  def rotate(angle: Double) = Point(x * cos(angle) - y * sin(angle), x * sin(angle) + y * cos(angle))

  override def toString = s"($x,$y)"

  def toSvgPoint(id: String) = <circle id={ id } cx={ x.toString } cy={ y.toString } r="0.4%"/>

  def toSvgCoordString = s"$x $y"
}