package shine.macros

import scala.annotation.{StaticAnnotation, compileTimeOnly}
import scala.reflect.macros.blackbox
import scala.language.experimental.macros

object Primitive {

  @compileTimeOnly("primitive macro")
  class expPrimitive extends StaticAnnotation {
    def macroTransform(annottees: Any*): Any = macro Impl.expPrimitive
  }

  class Impl(val c: blackbox.Context) {
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

    def makeLowerCaseName(s: String): String =
      Character.toLowerCase(s.charAt(0)) + s.substring(1)

    def makeVisitAndRebuild(name: TypeName,
                            additionalParams: List[ValDef],
                            params: List[ValDef]): Tree = {
      val v = q"v"
      q"""
        override def visitAndRebuild(
          $v: shine.DPIA.Phrases.VisitAndRebuild.Visitor): $name
        = new ${Apply( additionalParams match {
            case List() => Ident(name)
            case _ => Apply(Ident(name), additionalParams.map {
              case ValDef(_, name, _, _) => q"$name"
            })
          }, params.map {
          case ValDef(_, name, tpt, _) => tpt match {
              case Ident(TypeName("DataType")) | Ident(TypeName("ScalarType")) |
                   Ident(TypeName("BasicType"))     => q"$v.data($name)"
              case Ident(TypeName("Nat"))           => q"$v.nat($name)"
              case Ident(TypeName("NatToNat"))      => q"$v.natToNat($name)"
              case Ident(TypeName("NatToData"))     => q"$v.natToData($name)"
              case Ident(TypeName("AccessType"))    => q"$v.access($name)"
              case Ident(TypeName("AddressSpace"))  => q"$v.addressSpace($name)"
              // Phrase[ExpType]
              case AppliedTypeTree((Ident(TypeName("Phrase")), _)) =>
                q"shine.DPIA.Phrases.VisitAndRebuild($name, $v)"
              // Vector[Phrase[ExpType]]
              case AppliedTypeTree((Ident(TypeName("Vector")),
                  List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
                |   AppliedTypeTree((Ident(TypeName("Seq")),
                  List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
              =>
                q"$name.map(shine.DPIA.Phrases.VisitAndRebuild(_, $v))"
              case _ =>
                q"$name"
            }
          })}
      """
    }

    def makeXMLPrinter(name: TypeName,
                       additionalParams: List[ValDef],
                       params: List[ValDef]): Tree = {
      def makeAttributes(params: List[ValDef]): (List[ValDef], Tree) = {
        params.head match {
          case ValDef(_, name, tpt, _) => tpt match {
            case Ident(TypeName("DataType")) | Ident(TypeName("ScalarType")) |
                 Ident(TypeName("BasicType")) | Ident(TypeName("Nat")) |
                 Ident(TypeName("NatToNat")) | Ident(TypeName("NatToData")) |
                 Ident(TypeName("AccessType")) | Ident(TypeName("AddressSpace"))
              =>
              val (list, next) = makeAttributes(params.tail)
              (list, q"""
                 scala.xml.Attribute(${name.toString},
                                     scala.xml.Text(
                                        shine.DPIA.Phrases.ToString($name)),
                                     $next)
               """)
            case _ => (params, q"scala.xml.Null")
          }
        }
      }

      def makeBody(params: List[ValDef]): List[Tree] = {
        params.map {
          case ValDef(_, name, tpt, _) =>

            val body = tpt match {
              // Phrase[ExpType]
              case AppliedTypeTree((Ident(TypeName("Phrase")), _)) =>
                q"shine.DPIA.Phrases.xmlPrinter($name)"
              // Vector[Phrase[ExpType]]
              case AppliedTypeTree((Ident(TypeName("Vector")),
                  List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
                |   AppliedTypeTree((Ident(TypeName("Seq")),
                  List(AppliedTypeTree((Ident(TypeName("Phrase")), _)))))
              =>
                q"$name.flatMap(shine.DPIA.Phrases.xmlPrinter(_)):_*"
              case _ =>
                q"scala.xml.Text(shine.DPIA.Phrases.ToString($name))"
            }
            q"""
               scala.xml.Elem(null, ${name.toString},
                              scala.xml.Null, scala.xml.TopScope,
                              minimizeEmpty = false, $body)
             """
        }
      }

      val lowerCaseName = makeLowerCaseName(name.toString)
      val (rest, attributes) = makeAttributes(params)
      val body = makeBody(rest)

      q"""
       override def xmlPrinter: scala.xml.Elem = {
         val attributes = $attributes
         val body = $body
         scala.xml.Elem(null, $lowerCaseName, attributes, scala.xml.TopScope,
                        minimizeEmpty = false, body:_*)
       }
       """
    }

    def makeExpPrimitiveClass(name: TypeName,
                              additionalParams: List[ValDef],
                              params: List[ValDef],
                              body: List[Tree]): ClassDef = {
      var visitAndRebuildMissing = true
      var xmlPrinterMissing = true
      body.foreach {
        case DefDef(_, TermName("visitAndRebuild"), _, _, _, _) =>
          visitAndRebuildMissing = false
        case DefDef(_, TermName("xmlPrinter"), _, _, _, _) =>
          xmlPrinterMissing = false
        case _ =>
      }

      val generated = q"""
          ${if (visitAndRebuildMissing)
              makeVisitAndRebuild(name, additionalParams, params)
            else q""}

          ${if (xmlPrinterMissing)
              makeXMLPrinter(name, additionalParams, params)
            else q""}
         """

      val expClass = (additionalParams match {
        case List() =>
          q"""
          final case class $name(..$params) extends ExpPrimitive {
            ..$body
            ..$generated
          }
         """
        case _ =>
          val newParams = params map {
            case ValDef(_, name, tpt, rhs) if rhs.isEmpty => q"val $name : $tpt"
            case ValDef(_, name, tpt, rhs) => q"val $name : $tpt = $rhs"
          }
          q"""
          final case class $name(..$additionalParams)
                                (..$newParams) extends ExpPrimitive {
            ..$body
            ..$generated
          }
         """
      }).asInstanceOf[ClassDef]

//      println(expClass)
      expClass
    }

    def expPrimitivesFromClassDef: ClassDef => ClassDef = {
      case q"case class $name(..$params) extends $_ {..$body} " =>
        makeExpPrimitiveClass(
          name.asInstanceOf[c.TypeName],
          List(),
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]])
      case q"final case class $name(..$params) extends $_ {..$body} " =>
        makeExpPrimitiveClass(
          name.asInstanceOf[c.TypeName],
          List(),
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]])
      case q"""case class $name(..$additionalParams)
                               (..$params) extends $_ {..$body} """ =>
        makeExpPrimitiveClass(
          name.asInstanceOf[c.TypeName],
          additionalParams.asInstanceOf[List[ValDef]],
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]])
      case q"""final case class $name(..$additionalParams)
                                     (..$params) extends $_ {..$body} """ =>
        makeExpPrimitiveClass(
          name.asInstanceOf[c.TypeName],
          additionalParams.asInstanceOf[List[ValDef]],
          params.asInstanceOf[List[ValDef]],
          body.asInstanceOf[List[Tree]])
      case _ =>
        c.abort(c.enclosingPosition, "expected a case class extends Primitive")
    }
  }

}
