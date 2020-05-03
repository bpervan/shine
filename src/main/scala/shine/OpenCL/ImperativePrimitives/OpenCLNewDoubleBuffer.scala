package shine.OpenCL.ImperativePrimitives

import shine.DPIA.Phrases._
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class OpenCLNewDoubleBuffer(a: AddressSpace,
                                       dt1: DataType,
                                       dt2: DataType,
                                       dt3: DataType,
                                       n: Nat,
                                       in: Phrase[ExpType],
                                       out: Phrase[AccType],
                                       f: Phrase[(ExpType x AccType x CommType x CommType) ->: CommType])
  extends CommandPrimitive
{
  in :: expT(dt1, read)
  out :: accT(dt2)
  f :: (((varT(n`.`dt3) x comm) x comm) ->: comm)
}
