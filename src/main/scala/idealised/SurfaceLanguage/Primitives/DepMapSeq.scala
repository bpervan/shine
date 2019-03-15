package idealised.SurfaceLanguage.Primitives

import idealised.DPIA
import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Types._
import idealised.SurfaceLanguage._

//noinspection TypeAnnotation
final case class DepMapSeq(df: Expr[`(nat)->`[DataType -> DataType]], array: DataExpr,
                           override val t: Option[DataType])
  extends AbstractDepMap(df, array, t) {

  override def makeDPIAMap = DPIA.FunctionalPrimitives.DepMapSeq

  override def makeMap = DepMapSeq
}


//noinspection TypeAnnotation
final case class DepMapSeqUnroll(df: Expr[`(nat)->`[DataType -> DataType]], array: DataExpr,
                           override val t: Option[DataType])
  extends AbstractDepMap(df, array, t) {

  override def makeDPIAMap = DPIA.FunctionalPrimitives.DepMapSeqUnroll

  override def makeMap = DepMapSeqUnroll
}