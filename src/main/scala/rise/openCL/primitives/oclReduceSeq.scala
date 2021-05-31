// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package rise.openCL.primitives
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._
import arithexpr.arithmetic._
object oclReduceSeq extends Builder {
  private final case class Primitive()(override val t: Type = TypePlaceholder) extends rise.core.Primitive {
    override val name: String = "oclReduceSeq"
    override def setType(ty: Type): Primitive = Primitive()(ty)
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def typeScheme: Type = expl { (a: AddressSpace) => impl { (n: Nat) => impl { (s: DataType) => impl { (t: DataType) => (t ->: s ->: t) ->: t ->: ArrayType(n, s) ->: t } } } }
  }
  override def toString: String = "oclReduceSeq"
  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
