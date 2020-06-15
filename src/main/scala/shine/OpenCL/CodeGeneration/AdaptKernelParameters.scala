package shine.OpenCL.CodeGeneration

import shine.C.AST.ParamDecl
import shine.DPIA.DSL._
import shine.DPIA.FunctionalPrimitives.AsIndex
import shine.DPIA.Phrases._
import shine.DPIA.Types._
import shine.OpenCL.CodeGeneration.HoistMemoryAllocations.AllocationInfo
import shine.OpenCL.Primitives.OpenCL.OpenCLFunction
import shine.{C, DPIA, OpenCL}

import scala.collection.mutable

//
// Parameters (ParDecl) in global or local memory which have a non-array type in DPIA have to be
// represented as arrays of size 1 in OpenCL. Every reference to such a parameter is adjusted
// by indexing it with 0.
//
object AdaptKernelParameters {

  def apply(originalPhrase: Phrase[CommType],
            out: Identifier[AccType],
            ins: Seq[Identifier[ExpType]],
            intermediateAllocations: Seq[AllocationInfo],
            gen: CodeGenerator
           ): (Phrase[CommType], Identifier[AccType], Seq[Identifier[ExpType]], Seq[AllocationInfo], Seq[ParamDecl]) = {
    val params = makeParams(out, ins, intermediateAllocations, gen)
    val (newParams, scalarParamsInGlobalOrLocalMemory) = adaptParamDecls(params, ins)

    val rewrittenPhrase = VisitAndRebuild(originalPhrase, Visitor(scalarParamsInGlobalOrLocalMemory))

    def rewriteIdentifier[T <: PhraseType](x: Identifier[T]): Identifier[T] =
      if (scalarParamsInGlobalOrLocalMemory(x.name)) identifierAsSingletonArray(x) else x
    val rewrittenOut = rewriteIdentifier(out)
    val rewrittenIns = ins.map(rewriteIdentifier)
    val rewrittenIntermediates = intermediateAllocations.map(alloc => AllocationInfo(alloc.addressSpace,
      rewriteIdentifier(alloc.identifier)))

    (rewrittenPhrase, rewrittenOut, rewrittenIns, rewrittenIntermediates, newParams)
  }

  private def adaptParamDecls(params: Seq[(AddressSpace, ParamDecl)],
                              inputParams: Seq[Identifier[ExpType]]): (Seq[ParamDecl], Set[String]) = {
    val scalarParamsInGlobalOrLocalMemory = mutable.Set[String]()

    val newParams = params.map{ case (aSpace, paramDecl) =>
      paramDecl.t match {
        case _: C.AST.BasicType =>
          aSpace match {
            case AddressSpace.Global | AddressSpace.Local =>
              // remember scalar parameters in global or local memory and change their type to pointers
              scalarParamsInGlobalOrLocalMemory.add(paramDecl.name)
              ParamDecl(paramDecl.name,
                OpenCL.AST.PointerType(aSpace, paramDecl.t, const = inputParams.map(_.name).contains(paramDecl.name)))
            case _ => paramDecl
          }
        case at: C.AST.ArrayType =>
          // turn array types into flat pointers
          ParamDecl(paramDecl.name, OpenCL.AST.PointerType(aSpace, at.getBaseType,
            // ... and make input parameters const
            const = inputParams.map(_.name).contains(paramDecl.name)))

        case _ => paramDecl
      }
    }

    (newParams, scalarParamsInGlobalOrLocalMemory.toSet)
  }

  private case class Visitor(scalarParamsInGlobalOrLocalMemory: Set[String])
    extends VisitAndRebuild.Visitor
  {
    val zero: AsIndex = AsIndex(1, Natural(0))

    override def phrase[T <: PhraseType](p: Phrase[T]): Result[Phrase[T]] = {
      p match {
        case p1: Proj1[T, _] => p1.pair match {
          case i: Identifier[PhrasePairType[T, _]] if scalarParamsInGlobalOrLocalMemory.contains(i.name) =>
            i.`type` match {
              case PhrasePairType(_: ExpType, _: AccType) =>
                val j = identifierAsSingletonArray(i.asInstanceOf[Identifier[PhrasePairType[ExpType, AccType]]])
                Stop((Proj1(j) `@` zero).asInstanceOf[Phrase[T]])
              case _ => Continue(p, this)
            }
          case _ => Continue(p, this)
        }

        case p2: Proj2[_, T] => p2.pair match {
          case i: Identifier[PhrasePairType[_, T]] if scalarParamsInGlobalOrLocalMemory.contains(i.name) =>
            i.`type` match {
              case PhrasePairType(_: ExpType, _: AccType) =>
                val j = identifierAsSingletonArray(i.asInstanceOf[Identifier[PhrasePairType[ExpType, AccType]]])
                Stop((Proj2(j) `@` zero).asInstanceOf[Phrase[T]])
              case _ => Continue(p, this)
            }
          case _ => Continue(p, this)
        }

        case i: Identifier[T] if scalarParamsInGlobalOrLocalMemory.contains(i.name) =>
          Stop((i.`type` match {
            case _: ExpType =>
              identifierAsSingletonArray(i.asInstanceOf[Identifier[ExpType]]) `@` zero
            case _: AccType =>
              identifierAsSingletonArray(i.asInstanceOf[Identifier[AccType]]) `@` zero
          }).asInstanceOf[Phrase[T]])

        case f: OpenCLFunction => Continue(p, this)

        case _ => Continue(p, this)
      }
    }
  }

  private def identifierAsSingletonArray[T <: PhraseType](i: Identifier[T]): Identifier[T] = {
    i.`type` match {
      case _: ExpType =>
        val ie = i.asInstanceOf[Identifier[ExpType]]
        ie.copy(`type` = ExpType(DPIA.Types.ArrayType(1, ie.`type`.dataType), read)).asInstanceOf[Identifier[T]]
      case _: AccType =>
        val ia = i.asInstanceOf[Identifier[AccType]]
        ia.copy(`type` = AccType(DPIA.Types.ArrayType(1, ia.`type`.dataType))).asInstanceOf[Identifier[T]]
      case PhrasePairType(_: ExpType, _: AccType) =>
        val ip = i.asInstanceOf[Identifier[PhrasePairType[ExpType, AccType]]]
        ip.copy(`type` = PhrasePairType(
          ExpType(DPIA.Types.ArrayType(1, ip.`type`.t1.dataType), read),
          AccType(DPIA.Types.ArrayType(1, ip.`type`.t2.dataType)))).asInstanceOf[Identifier[T]]
    }
  }

  private def makeParams(out: Identifier[AccType],
                         ins: Seq[Identifier[ExpType]],
                         intermediateAllocations: Seq[AllocationInfo],
                         gen: CodeGenerator): Seq[(AddressSpace, ParamDecl)] = {
    Seq(makeGlobalParam(out, gen)) ++ // first the output parameter ...
      ins.map(makeInputParam(_, gen)) ++ // ... then the input parameters ...
      intermediateAllocations.map(makeParam(_, gen)) //++  ... then the intermediate buffers ...
  }

  // pass arrays via global and scalar + tuple values via private memory
  private def makeInputParam(i: Identifier[_], gen: CodeGenerator): (AddressSpace, ParamDecl) = {
    getDataType(i) match {
      case _: ArrayType => makeGlobalParam(i, gen)
      case _: DepArrayType => makeGlobalParam(i, gen)
      case _: BasicType => makePrivateParam(i, gen)
      case _: PairType => makePrivateParam(i, gen)
      case _: DataTypeIdentifier => throw new Exception("This should not happen")
      case _: NatToDataApply =>  throw new Exception("This should not happen")
    }
  }

  private def makeGlobalParam(i: Identifier[_], gen: CodeGenerator): (AddressSpace, ParamDecl) = {
    (AddressSpace.Global, ParamDecl(i.name,gen.typ(getDataType(i))))
  }

  private def makePrivateParam(i: Identifier[_], gen: CodeGenerator): (AddressSpace, ParamDecl) = {
    (AddressSpace.Private, ParamDecl(i.name, gen.typ(getDataType(i))))
  }

  private def makeParam(allocInfo: AllocationInfo, gen: CodeGenerator): (AddressSpace, ParamDecl) = {
    (allocInfo.addressSpace, ParamDecl(allocInfo.identifier.name, gen.typ(getDataType(allocInfo.identifier))))
  }

  private def getDataType(i: Identifier[_]): DataType = i.t match {
    case ExpType(dataType, _) => dataType
    case AccType(dataType) => dataType
    case PhrasePairType(ExpType(dt1, _), AccType(dt2)) if dt1 == dt2 => dt1
    case _ => throw new Exception("This should not happen")
  }
}
