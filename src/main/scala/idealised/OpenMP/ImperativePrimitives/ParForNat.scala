package idealised.OpenMP.ImperativePrimitives

import idealised.DPIA.Phrases._
import idealised.DPIA.Types._
import idealised.DPIA._

//noinspection TypeAnnotation
final case class ParForNat(override val n: Nat,
                           override val ft:NatDataTypeFunction,
                           override val out: Phrase[AccType],
                           override val body: Phrase[`(nat)->`[AccType -> CommandType]])
  extends AbstractParForNat(n, ft, out, body) {
  override def makeParForNat = ParForNat
}