package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.Phrases.{CommandPrimitive, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ParForNat(level: ParallelismLevel,
                           dim: Int,
                           unroll: Boolean = false)
                          (val n: Nat,
                           val ft: NatToData,
                           val out: Phrase[AccType],
                           val loopBody: Phrase[`(nat)->:`[AccType ->: CommType]])
  extends CommandPrimitive
{
  out :: accT(n`.d`ft)
  loopBody :: loopBody.t.x ->: accT(ft(loopBody.t.x)) ->: comm

  def init: Nat = level match {
    case Global => get_global_id(dim)
    case Local => get_local_id(dim)
    case WorkGroup => get_group_id(dim)
    case Sequential => ???
  }

  def step: Nat = level match {
    case Global => get_global_size(dim)
    case Local => get_local_size(dim)
    case WorkGroup => get_num_groups(dim)
    case Sequential => ???
  }

  def name: String = level match {
    case Global => freshName("gl_id_")
    case Local => freshName("l_id_")
    case WorkGroup => freshName("wg_id_")
    case Sequential => ???
  }
}
