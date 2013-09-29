package models

import math.{ sqrt, Pi, pow, acos, sin, abs }

class VennDiagram(params: VennDiagramParameters) {

  private def square(x: Double) = pow(x, 2)

  // initialize circles and intersections
  val (circleA, circleB, circleC, intersectionAB, intersectionBC, intersectionCA) = {

    // start with idealized circles, which just have a name and an area, but no center
    val idealCircleA = IdealCircle("A", params.aArea)
    val idealCircleB = IdealCircle("B", params.bArea)
    val idealCircleC = IdealCircle("C", params.cArea)

    // and with idealized circle intersections, which are defined in terms of idealized circles
    val idealIntersectionAB = IdealCircleIntersection(idealCircleA, idealCircleB, params.abArea)
    val idealIntersectionBC = IdealCircleIntersection(idealCircleB, idealCircleC, params.bcArea)
    val idealIntersectionCA = IdealCircleIntersection(idealCircleC, idealCircleA, params.caArea)

    // use shorthands for "sep" values -- these will define the triangle of circle centers
    val ab = idealIntersectionAB.sep
    val bc = idealIntersectionBC.sep
    val ca = idealIntersectionCA.sep

    // define actual circles (with locations based on a center)
    // and actual intersections (with locations and rotations)

    // circle A and intersection AB

    val centerA = Point(0, 0) // put A at the origin
    val circleA = idealCircleA.realizeWith(centerA)
    val intersectionABrotation = 0

    val intersectionAB = idealIntersectionAB.realizeWith(centerA, intersectionABrotation)

    // circle B and intersection BC

    val centerB = Point(ab, 0) // on x-axis to the right of A
    val circleB = idealCircleB.realizeWith(centerB)
    val intersectionBCrotation = if ((ab == 0) || (bc == 0)) intersectionAB.rotation else {
      val cosOfAngle = (square(ab) + square(bc) - square(ca)) / (2 * ab * bc)
      if (abs(cosOfAngle) > 1)
        throw new RuntimeException("Geometry found inconsistent when calculating B\u2229C rotation")
      intersectionAB.rotation + Pi - acos(cosOfAngle)
    }

    val intersectionBC = idealIntersectionBC.realizeWith(centerB, intersectionBCrotation)

    // circle C and intersection CA

    val x = if (ab == 0) circleA.center.x else {
      (square(ca) + square(ab) - square(bc)) / (2 * ab)
    }

    val squareOfY = square(ca) - square(x)
    if (squareOfY < 0)
      throw new RuntimeException("Geometry found inconsistent when calculating C center")

    val centerC = Point(x, sqrt(squareOfY))
    val circleC = idealCircleC.realizeWith(centerC)
    val intersectionCArotation = if ((ca == 0) || (bc == 0)) intersectionBC.rotation else {
      val cosOfAngle = (square(bc) + square(ca) - square(ab)) / (2 * bc * ca)
      if (abs(cosOfAngle) > 1)
        throw new RuntimeException("Geometry found inconsistent when calculating C\u2229A rotation")
      intersectionBC.rotation + Pi - acos(cosOfAngle)
    }

    val intersectionCA = idealIntersectionCA.realizeWith(centerC, intersectionCArotation)

    (circleA, circleB, circleC, intersectionAB, intersectionBC, intersectionCA)
  }

  // now that all the circles are defined, 
  // the boundaries of the grid can be established
  val gridDimension = GridDimension(circleA, circleB, circleC);

  val intersectionABC = {
    //    if (circleA.contains(circleB)||circleA.contains(circleC)) Scalar.format(intersectionBC.area)
    //    else if (circleB.contains(circleA)||circleB.contains(circleC)) Scalar.format(intersectionCA.area)
    //    else if (circleC.contains(circleA)||circleC.contains(circleB)) Scalar.format(intersectionAB.area)
    //    else generalIntersectionABC
    if (circleA.contains(intersectionBC)) Scalar.format(intersectionBC.area)
    else if (circleB.contains(intersectionCA)) Scalar.format(intersectionCA.area)
    else if (circleC.contains(intersectionAB)) Scalar.format(intersectionAB.area)
    else generalIntersectionABC
  }

  private def generalIntersectionABC = {
    def segmentArea(c: Circle, chord: LineSegment, otherChordPoint: Point) = {
      val angle = 2 * acos(c.center.distanceTo(chord.mid) / c.radius)
      val area = square(c.radius) / 2 * (angle - sin(angle))
      if (LineSegment(c.center, otherChordPoint).contains(chord.mid)) c.area - area else area
    }

    def triangleArea(a: Double, b: Double, c: Double) = { // Heron's'
      val s = (a + b + c) / 2
      sqrt(s * (s - a) * (s - b) * (s - c))
    }

    //shorthand for chord interior points
    val abP = intersectionAB.chord.p1
    val bcP = intersectionBC.chord.p1
    val caP = intersectionCA.chord.p1

    //this doesn't always work
    if (circleA.contains(bcP) && circleB.contains(caP) && circleC.contains(abP)) {
      Scalar.format(
        segmentArea(circleA, LineSegment(abP, caP), bcP) +
          segmentArea(circleB, LineSegment(bcP, abP), caP) +
          segmentArea(circleC, LineSegment(caP, bcP), abP) +
          triangleArea(abP.distanceTo(bcP), bcP.distanceTo(caP), caP.distanceTo(abP))
      ) + " = " +
        Scalar.format(segmentArea(circleA, LineSegment(abP, caP), bcP)) + " + " +
        Scalar.format(segmentArea(circleB, LineSegment(bcP, abP), caP)) + " + " +
        Scalar.format(segmentArea(circleC, LineSegment(caP, bcP), abP)) + " + " +
        Scalar.format(triangleArea(abP.distanceTo(bcP), bcP.distanceTo(caP), caP.distanceTo(abP)))
    } else "0"
  }

  def toSvgIntersectionABC = if (intersectionABC == 0) "Unknown" else intersectionABC

  def toSvgCircles =
    <g>
      { circleA.toSvgCircle }
      { circleB.toSvgCircle }
      { circleC.toSvgCircle }
    </g>

  def toSvgCenters =
    <g>
      { circleA.toSvgCenter }
      { circleB.toSvgCenter }
      { circleC.toSvgCenter }
    </g>

  def toSvgChords =
    <g>
      { intersectionAB.toSvgChord }
      { intersectionBC.toSvgChord }
      { intersectionCA.toSvgChord }
    </g>

}

object VennDiagram {
  def apply(params: VennDiagramParameters) = new VennDiagram(params)
}