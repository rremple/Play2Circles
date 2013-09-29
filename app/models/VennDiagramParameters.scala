package models

case class VennDiagramParameters(
    aArea: Int,
    bArea: Int,
    cArea: Int,
    abArea: Int,
    bcArea: Int,
    caArea: Int) {
  val errors = List(
    if (abArea > math.min(aArea, bArea)) Some("Invalid intersetion of A and B") else None,
    if (bcArea > math.min(bArea, cArea)) Some("Invalid intersetion of B and C") else None,
    if (caArea > math.min(cArea, aArea)) Some("Invalid intersetion of C and A") else None
  ).flatten

}