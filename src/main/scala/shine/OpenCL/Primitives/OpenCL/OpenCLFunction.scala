package shine.OpenCL.Primitives.OpenCL

import shine.DPIA.Compilation.TranslationContext
import shine.DPIA.Compilation.TranslationToImperative.con
import shine.DPIA.DSL.{λ, _}
import shine.DPIA.Phrases.{ExpPrimitive, Phrase, PrettyPhrasePrinter}
import shine.DPIA.Types.{AccType, CommType, DataType, ExpType, read, _}
import shine.DPIA.{->:, expT}
import shine.macros.Primitive.expPrimitive

@expPrimitive
final case class OpenCLFunction(name: String,
                                inTs: Seq[DataType],
                                outT: DataType,
                                args: Seq[Phrase[ExpType]]) extends ExpPrimitive
{
  (inTs zip args).foreach {
    case (inT, arg) => arg :: expT(inT, read)
  }
  override val t: ExpType = expT(outT, read)

  override def prettyPrint: String =
    s"$name(${args.map(PrettyPhrasePrinter(_)).mkString(",")})"

  override def acceptorTranslation(A: Phrase[AccType])
                                  (implicit context: TranslationContext
                                  ): Phrase[CommType] = {
    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                exps: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process return the assignment of the
        // OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(expT(inT, read))(e =>
            A :=|outT| OpenCLFunction(name, inTs :+ inT, outT, exps :+ e) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e =>
            recurse(tail, exps :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }

  override def continuationTranslation(C: Phrase[->:[ExpType, CommType]])
                                      (implicit context: TranslationContext
                                      ): Phrase[CommType] = {
    def recurse(ts: Seq[(Phrase[ExpType], DataType)],
                es: Seq[Phrase[ExpType]],
                inTs: Seq[DataType]): Phrase[CommType] = {
      ts match {
        // with only one argument left to process continue with the
        // OpenCLFunction call
        case Seq( (arg, inT) ) =>
          con(arg)(λ(expT(inT, read))(e =>
            C(OpenCLFunction(name, inTs :+ inT, outT, es :+ e)) ))
        // with a `tail` of arguments left, recurse
        case Seq( (arg, inT), tail@_* ) =>
          con(arg)(λ(expT(inT, read))(e =>
            recurse(tail, es :+ e, inTs :+ inT) ))
      }
    }

    recurse(args zip inTs, Seq(), Seq())
  }
}
