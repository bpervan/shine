package idealised.OpenCL.SurfaceLanguage.Primitives

import idealised.SurfaceLanguage.DSL.DataExpr
import idealised.SurfaceLanguage.Primitives.AbstractDepMap
import idealised.SurfaceLanguage.Types.DataType
import idealised.SurfaceLanguage.{->, Expr, `(nat)->`}

final case class DepMapWorkGroup(dim:Int)(f: Expr[`(nat)->`[DataType -> DataType]], array: DataExpr,
                                       override val t: Option[DataType] = None)
  extends AbstractDepMap(f, array, t)
{
  override def makeMap = DepMapWorkGroup(dim) _

  override def makeDPIAMap = idealised.OpenCL.FunctionalPrimitives.DepMapWorkGroup(dim)
}