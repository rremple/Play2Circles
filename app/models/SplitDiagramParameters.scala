package models

case class SplitDiagramParameters(
    totalArea: Int,
    aArea: Int,
    bArea: Int,
    abArea: Int) {
  val errors = List(
    if (aArea > totalArea) Some("Invalid subset A of total") else None,
    if (bArea > totalArea) Some("Invalid subset B of total") else None,
    if (abArea > math.min(aArea, bArea)) Some("Invalid intersetion of A and B") else None
  ).flatten

}