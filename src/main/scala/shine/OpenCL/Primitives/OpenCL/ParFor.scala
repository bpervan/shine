package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.Phrases.{CommandPrimitive, _}
import shine.DPIA.Types.DataType._
import shine.DPIA.Types._
import shine.DPIA._
import shine.OpenCL._
import shine.macros.Primitive.comPrimitive

@comPrimitive
final case class ParFor(level: ParallelismLevel,
                        dim: Int,
                        unroll: Boolean = false)
                       (val n: Nat,
                        val dt: DataType,
                        val out: Phrase[AccType],
                        val loopBody: Phrase[ExpType ->: AccType ->: CommType],
                        val init: Nat = level match {
                          case Global => get_global_id(dim)
                          case Local => get_local_id(dim)
                          case WorkGroup => get_group_id(dim)
                          case Sequential => ???
                        },
                        val step: Nat = level match {
                          case Global => get_global_size(dim)
                          case Local => get_local_size(dim)
                          case WorkGroup => get_num_groups(dim)
                          case Sequential => ???
                        },
                        val name: String = level match {
                          case Global => freshName("gl_id_")
                          case Local => freshName("l_id_")
                          case WorkGroup => freshName("wg_id_")
                          case Sequential => ???
                        }) extends CommandPrimitive
{
  out :: accT(n`.`dt)
  loopBody :: expT(idx(n), read) ->: accT(dt) ->: comm
}
