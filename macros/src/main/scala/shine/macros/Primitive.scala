package shine.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

object Primitive {

  @compileTimeOnly("primitive macro")
  class expPrimitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.expPrimitive
  }

  class IdentityImpl(val c: blackbox.Context) {
    def impl(annottees: c.Expr[Any]*): c.Expr[Any] = {
      import c.universe._
      val inputs = annottees.map(_.tree).toList
      val (annottee, expandees) = inputs match {
        case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
        case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
        case _ => (EmptyTree, inputs)
      }
      println((annottee, expandees))
      val outputs = expandees
      c.Expr[Any](Block(outputs, Literal(Constant(()))))
    }
  }

  class Impl(val c: blackbox.Context) {
//    import c.universe.Flag._
    import c.universe._

    def expPrimitive(annottees: c.Expr[Any]*): c.Expr[Any] = {
      annottees.map(_.tree) match {
        case (cdef: ClassDef) :: Nil =>
          c.Expr(expPrimitivesFromClassDef(cdef))
        case (cdef: ClassDef) :: (md: ModuleDef) :: Nil =>
          c.Expr(q"{${expPrimitivesFromClassDef(cdef)}; $md}")
        case _ => c.abort(c.enclosingPosition, "expected a class definition")
      }
    }
    def makeStringName(s: String): String =
      Character.toLowerCase(s.charAt(0)) + s.substring(1)

    def makeArgs(p: Seq[Tree]): Seq[TermName] =
      p.map({
        case q"$_ val $n: $_ "     => n
        case q"$_ val $n: $_ = $_" => n
        case x =>
          c.abort(c.enclosingPosition, s"expected a parameter, but got $x")
      })
        .asInstanceOf[Seq[TermName]]

//    def makeArgList(seq: Seq[TermTree]): Tree = seq.size match {
//      case 0 => q""
//      case 1 => q"${seq.head}"
//      case _ => q"${seq.head} , ${makeArgList(seq.tail)}"
//    }
//
    def makeVisitAndRebuild(name: TypeName, args: List[Ident]): Tree = {
      def makeCall: Apply = Apply(Ident(name), args)

      q"""
      override def visitAndRebuild(v: VisitAndRebuild.Visitor): $name
        = ${makeCall}
      """
    }

    def makeChain(a: TermName, props: Seq[TermName]): Tree =
      if (props.isEmpty) q"true"
      else
        q"(${a}.${props.head} == ${props.head}) && ${makeChain(a, props.tail)}"

    def makeExpPrimitiveClass(name: TypeName,
                              params: Seq[Tree],
                              body: Seq[Tree]): ClassDef = {

      val paramIdentifiers: List[Ident] = params.map {
        case ValDef(_, name, _, _) => Ident(name)
      }.toList
      println(makeVisitAndRebuild(name, paramIdentifiers))

      val expClass = q"""
          final case class $name(..$params) extends ExpPrimitive {
            ..$body

            ${makeVisitAndRebuild(name, paramIdentifiers)}
          }
         """.asInstanceOf[ClassDef]

      println(expClass)
      expClass
    }

    def expPrimitivesFromClassDef: ClassDef => ClassDef = {
      case q"case class $name(..$params) extends $_ {..$body} " =>
        makeExpPrimitiveClass(name.asInstanceOf[c.TypeName], params.asInstanceOf[Seq[Tree]], body.asInstanceOf[Seq[Tree]])
      case _ =>
        c.abort(c.enclosingPosition, "expected a case class extends Primitive")
    }
  }

}
