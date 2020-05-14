package example

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class generateWriterT extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro identityMacro.impl
}

object identityMacro {
  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    def modifiedDef(defdef: DefDef): c.Expr[Any] = {
//      println( showCode(defdef) )

      defdef.children.last match {
        case Block((anonCls: ClassDef) :: Nil, b) =>
          val interested = anonCls.children.flatMap(_.children).collect {
            case DefDef(mods, name, tparams, vparamss, tpt, rhs) => rhs
          }

//          println(interested)
//          println((anonCls, b))
      }

//      val q"def $name = $x" = defdef
//      println(name)
//      println(tparam)
//      println(x)

      val DefDef(mods, name: TermName, tparams, vparamss, tpt, rhs) = defdef

//      println(rhs)

      val applyWriterT = rhs match {
        case Block(trees, tree) =>
          Block(
            trees.map {
              case ClassDef(modifiers, typeName, typeDefs, template @ Template(Ident(parents), self, body)) =>
//                println(body)
//                println(parents)
                tparams.map { td =>
                  td.symbol
                }

                body.foreach {
                  case d @ DefDef(mods, name, _, vparamss, tpt, rhs) =>
                    tpt match {
                      case AppliedTypeTree(Ident(name), applied) =>
                        if (name == tparams.head.name) {
                          rhs match {
                            case Block(stmts, expr) =>
                              val quoted = q"""
                                 val lifted = _root_.cats.data.WriterT.liftF[F, _root_.scala.List[_root_.example.Call], ${applied.head}]($expr)
                                 lifted.tell(List(Call("bar", List(s1, s2))))
                                 """

//                              println(quoted)
//                              println(expr)
                            case Apply(fun, args) => println(fun)
                          }

                        }

                      case t => //println(defdef)
                    }
                  case _ => c.abort(c.enclosingPosition, "bad!")

                }

                ClassDef(modifiers, typeName, typeDefs, Template(body, self, body))
              case t => t
            },
            tree
          )
        case _ => c.abort(c.enclosingPosition, "No class!")
      }

      val writerTApplied = DefDef(
        mods,
        TermName("ab"),
        tparams,
        vparamss,
        tpt,
        applyWriterT
      )

      c.Expr[Any](
        q"""$defdef; $writerTApplied"""
      )
    }

    annottees.map(_.tree) match {
      case (classDecl: DefDef) :: Nil => modifiedDef(classDecl)
//      case (classDecl: ClassDef) :: (compDecl: ModuleDef) :: Nil => modifiedClass(classDecl, Some(compDecl))
      case _ => c.abort(c.enclosingPosition, "Invalid annottee")
    }

//    val inputs = annottees.map(_.tree).toList
//    val (annottee, expandees) = inputs match {
//      case (param: ValDef) :: (rest @ (_ :: _)) => (param, rest)
//      case (param: TypeDef) :: (rest @ (_ :: _)) => (param, rest)
//      case _ => (EmptyTree, inputs)
//    }
//    println((annottee, expandees))
//    val outputs = expandees
//    c.Expr[Any](Block(outputs, Literal(Constant(()))))

  }
}
