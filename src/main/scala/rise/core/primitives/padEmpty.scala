// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
// This file is automatically generated and should not be changed manually //
// !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! //
package rise.core.primitives
import rise.core.DSL._
import rise.core.DSL.Type._
import rise.core._
import rise.core.types._
import arithexpr.arithmetic._
object padEmpty extends Builder {
  private final case class Primitive()(override val t: Type = TypePlaceholder) extends rise.core.Primitive {
    override val name: String = "padEmpty"
    override def setType(ty: Type): Primitive = Primitive()(ty)
    override def primEq(obj: rise.core.Primitive): Boolean = obj.getClass == getClass
    override def typeScheme: Type = impl { (n: Nat) => expl { (r: Nat) => impl { (t: DataType) => ArrayType(n, t) ->: ArrayType(n + r, t) } } }
  }
  override def toString: String = "padEmpty"
  override def primitive: rise.core.Primitive = Primitive()()
  override def apply: ToBeTyped[rise.core.Primitive] = toBeTyped(Primitive()())
  override def unapply(arg: Expr): Boolean = arg match {
    case _: Primitive => true
    case _ => false
  }
}
