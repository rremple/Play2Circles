package models

import math.{ Pi, sqrt, pow, abs, min, max, cos, sin, tan, acos }

class SplitDiagram(params: SplitDiagramParameters) {

  private def square(x: Double) = pow(x, 2)

  // initialize circles and intersections
  val o = Point(0, 0) // origin
  val circle = IdealCircle("Total", params.totalArea)
  val rCircle = circle.realizeWith(o)

  val alphaChord = circle.chord(areaTarget = params.aArea)
  val alpha = alphaChord.p1.x
  //  val a = Point(alpha, 0)
  //  val oaSeg = LineSegment(o, a)

  val betaChordRaw = circle.chord(areaTarget = params.bArea) // used to determine theta
  val beta = betaChordRaw.p1.x
  //  val bRaw = Point(beta, 0)

  /*
   * returns the area and the intersection point as a pair
   */
  def abAttributes(theta: Double) = {
    val betaChord = betaChordRaw.rotate(theta)
    val p = Point(alpha, ((beta / cos(theta) - alpha) / tan(theta)))
    val pAlpha = LineSegment(p, alphaChord.p2)
    val pBeta = LineSegment(p, betaChord.p1)
    val newChord = LineSegment(betaChord.p1, alphaChord.p2)
    (triangleArea(pAlpha.length, pBeta.length, newChord.length) + circle.chordAttributes(newChord)._1, p)
  }

  val zeroAreaTheta = -(acos(alpha / circle.radius) + acos(beta / circle.radius))
  val theta = Solver.solveForIncreasing(zeroAreaTheta, 0, params.abArea) { abAttributes(_)._1 }
  val intersectionPoint = abAttributes(theta)._2

  //  val b = bRaw.rotate(theta)
  //  val obSeg = LineSegment(o, b)
  val betaChord = betaChordRaw.rotate(theta)

  def triangleArea(ap: Double, bp: Double, cp: Double) = { // Numerically-stable Heron's
    val a = max(ap, max(bp, cp))
    val c = min(ap, min(bp, cp))
    val b = (ap + bp + cp) - a - c
    sqrt((a + (b + c)) * (c - (a - b)) * (c + (a - b)) * (a + (b - c))) / 4
  }

  val gridDimension = GridDimension(rCircle);

  def toSvgAttributes = f"radius = ${circle.radius}%2.2f, alpha = $alpha%2.2f, beta = $beta%2.2f" +
    f", theta = ${-theta / Pi * 180}%2.2f" +
    f", intersection at (${intersectionPoint.x}%2.2f,${-intersectionPoint.y}%2.2f)"

  def toSvgCircle =
    <g>{ rCircle.toSvgCircle }</g>

  def toSvgCenters =
    <g>
      { rCircle.toSvgCenter }
      { intersectionPoint.toSvgPoint("p") }
    </g>

  def toSvgChords =
    <g>
      { alphaChord.toSvg("alpha") }
      { betaChord.toSvg("beta") }
    </g>
  //{ LineSegment(betaChord.p1, alphaChord.p2).toSvg("alphaChord") }
}

object SplitDiagram {
  def apply(params: SplitDiagramParameters) = new SplitDiagram(params)
}