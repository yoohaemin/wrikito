package example

import scala.annotation.StaticAnnotation
import scala.annotation.compileTimeOnly
import scala.reflect.macros.whitebox

/**
  * Requires Applicative[F] instance for F (for calling WriterT.liftF)
  */
@compileTimeOnly("Enable macro paradise to expand macro annotations")
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

    def transformClassDef(impl: ClassDef): ClassDef = {
      val ClassDef(modifiers, typeName, typeDefs, Template(parents, self, body)) = impl

      val (superClassName, classEffectType) = parents match {
        case AppliedTypeTree(superClassName, classEffectType :: Nil) :: _ => (superClassName, classEffectType)
        case _ =>
          c.abort(c.enclosingPosition, "Class definition should extend a class/trait parameterized with one type")
      }

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
          def createWriterTLiftedTree(expr: Tree) =
            q"""
                def __eval_result: _root_.scala.Any = $expr

                type LocalReturnEffectType[A] = $localReturnEffectType[A]
                type ResultType = $resultType

                val __writerT_applied_result:  _root_.cats.data.WriterT[LocalReturnEffectType, List[Call], ResultType] = __eval_result match {

                   case alreadyLifted: _root_.cats.data.WriterT[LocalReturnEffectType, List[Call], ResultType] @unchecked =>
                     alreadyLifted.tell(_root_.scala.List(_root_.example.Call(${name.encodedName.toString}, ${vparamss.map(_.map(_.name))})))

                   case needsLifting: LocalReturnEffectType[ResultType] @unchecked =>
                     val __writerT_lifted = _root_.cats.data.WriterT.liftF[LocalReturnEffectType, _root_.scala.List[_root_.example.Call], ResultType](needsLifting)
                     __writerT_lifted.tell(_root_.scala.List(_root_.example.Call(${name.encodedName.toString}, ${vparamss.map(_.map(_.name))})))
                }
                __writerT_applied_result
             """

          //Rewrite the method body
          val writerTAppliedRhs = rhs match {
            case Block(stmts, expr) =>
              val Block(lifted, result) = createWriterTLiftedTree(expr)
              Block(stmts ++ lifted, result)
            case expr =>
              createWriterTLiftedTree(expr)
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

      ClassDef(modifiers, typeName, typeDefs, Template(writerTAppliedParent :: parents.tail, self, writerTAppliedBody))
    }

    def buildWriterTImplDef(defdef: DefDef): c.Expr[Any] = {
      val DefDef(mods, name: TermName, tparams, vparamss, tpt, rhs) = defdef

      val (superClassName, classEffectType) = tpt match {
        case AppliedTypeTree(superClassName, classEffectType :: Nil) => (superClassName, classEffectType)
        case _ =>
          c.abort(c.enclosingPosition, "Wrikito currently only works on methods with explicitly specified types.")
      }

      val applyWriterT = rhs match {
        case Block(trees, tree)
            if trees.exists(tree => showRaw(tree).startsWith("ClassDef")) => //TODO find a better way
          Block(
            trees.map {
              case c: ClassDef => transformClassDef(c)
              case t           => t
            },
            tree
          )
        case _ => c.abort(c.enclosingPosition, "Rhs of the annotated method definition must be a class implementation.")
      }

      val writerTAppliedValueParams = vparamss.map(_.map {
        case valDef @ ValDef(mods, name, tt @ AppliedTypeTree(tpe, effect :: Nil), rhs)
            if !mods.hasFlag(Flag.IMPLICIT) &&
              classEffectType.equalsStructure(effect) =>
          val writerTAppliedParamType = treeCopy.AppliedTypeTree(tt, tpe, writerTAppliedType(effect) :: Nil)
          treeCopy.ValDef(valDef, mods, name, writerTAppliedParamType, rhs)
        case other => other
      })

      val writerTAppliedReturnType =
        treeCopy.AppliedTypeTree(tpt, superClassName, writerTAppliedType(classEffectType) :: Nil)

      val writerTApplied = DefDef(
        mods,
        TermName(name.encodedName.toString + "_writerT"),
        tparams,
        writerTAppliedValueParams,
        writerTAppliedReturnType,
        applyWriterT
      )

      c.Expr[Any](q"""$defdef; $writerTApplied""")
    }

    annottees.map(_.tree) match {
      case (algebraImpl: DefDef) :: Nil => buildWriterTImplDef(algebraImpl)
      case _                            => c.abort(c.enclosingPosition, "Wrikito currently only works on `def`s.")
    }
  }
}
