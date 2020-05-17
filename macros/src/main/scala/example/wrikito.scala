package example

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox

@compileTimeOnly("enable macro paradise to expand macro annotations")
class wrikito extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro wrikito.impl
}

object wrikito {

  def impl(c: whitebox.Context)(annottees: c.Expr[Any]*): c.Expr[Any] = {
    import c.universe._

    /**
     * Build a WriterT wrapped type constructor from a given type constructor.
     *
     * @param typeName `F` as in `Algebra[F]` or `F[Int]`
     * @return `WriterT[F, List[Call], *]`
     */
    def writerTAppliedType(typeName: Tree): Tree =
      SelectFromTypeTree(
        CompoundTypeTree(
          Template(
            List(Select(q"_root_.scala", TypeName("AnyRef"))),
            noSelfType,
            List(
              TypeDef(
                Modifiers(),
                TypeName("X"),
                List(TypeDef(Modifiers(Flag.PARAM), TypeName("A"), List(), TypeBoundsTree(EmptyTree, EmptyTree))),
                AppliedTypeTree(
                  Select(q"_root_.cats.data", TypeName("WriterT")),
                  List(
                    typeName,
                    AppliedTypeTree(
                      Select(
                        q"_root_.scala",
                        TypeName("List")
                      ),
                      List(
                        Select(
                          q"_root_.example",
                          TypeName("Call")
                        )
                      )
                    ),
                    Ident(TypeName("A")),
                  )
                )
              )
            )
          )
        ),
        TypeName("X")
      )

    def addWriterT(impl: ClassDef): ClassDef = {
      val ClassDef(modifiers, typeName, typeDefs, Template(parents, self, body)) = impl
      val AppliedTypeTree(superClassName, classEffectType :: _)                  = parents.head

      val writerTAppliedParent = AppliedTypeTree(superClassName, writerTAppliedType(classEffectType) :: Nil)

      def contains(flagSet: FlagSet, flag: FlagSet): Boolean =
        (flagSet | flag) == flagSet

      val writerTAppliedBody = body.map {
        case DefDef(
            mods @ Modifiers(flagSet, _, _),
            name,
            tparams: List[TypeDef],
            vparamss: List[List[ValDef]],
            AppliedTypeTree(localReturnEffectType, resultType :: Nil),
            rhs: Tree
            ) if !contains(flagSet, Flag.PRIVATE) =>
          def base(expr: Tree) =
            q"""
                 val __writerT_lifted = _root_.cats.data.WriterT.liftF[$localReturnEffectType, _root_.scala.List[_root_.example.Call], $resultType]($expr)
                 __writerT_lifted.tell(_root_.scala.List(_root_.example.Call(${name.encodedName.toString}, ${vparamss
              .map(_.map(_.name))})))
          """

          //Rewrite the method body
          val writerTAppliedRhs = rhs match {
            case Block(stmts, expr) =>
              val Block(lifted, result) = base(expr)

              Block(stmts ++ lifted, result)

            case expr: Apply =>
              base(expr)
          }

          DefDef(
            mods,
            name,
            tparams,
            vparamss,
            AppliedTypeTree(writerTAppliedType(localReturnEffectType), resultType :: Nil),
            writerTAppliedRhs
          )
        case other => other
      }

      //writerTAppliedParent
      ClassDef(modifiers, typeName, typeDefs, Template(writerTAppliedParent :: parents.tail, self, writerTAppliedBody))
    }

    def modifiedDef(defdef: DefDef): c.Expr[Any] = {
      val DefDef(mods, name: TermName, tparams, vparamss, tpt, rhs) = defdef
      val AppliedTypeTree(superClassName, classEffectType :: Nil)   = tpt

      val applyWriterT = rhs match {
        case Block(trees, tree) =>
          Block(
            trees.map {
              case c: ClassDef => addWriterT(c)
              case t           => t
            },
            tree
          )
        case _ => c.abort(c.enclosingPosition, "No class!")
      }

      val writerTApplied = DefDef(
        mods,
        TermName(name.encodedName.toString + "_writerT"),
        tparams,
        vparamss,
        AppliedTypeTree(superClassName, writerTAppliedType(classEffectType) :: Nil),
        applyWriterT
      )

      c.Expr[Any](
        q"""$defdef; $writerTApplied"""
      )
    }



    println(
      annottees
    )
    annottees.map(_.tree) match {
      case (classDecl: DefDef) :: Nil => modifiedDef(classDecl)
      case _                          => c.abort(c.enclosingPosition, "Invalid annottee")
    }
  }
}
