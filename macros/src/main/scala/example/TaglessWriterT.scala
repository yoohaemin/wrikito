package cats.tagless

import scala.annotation.{ compileTimeOnly, StaticAnnotation }
import scala.collection.immutable.Seq
import scala.reflect.macros.whitebox

/** Auto generates an instance of [[FunctorK]]. */
@compileTimeOnly("Cannot expand @autoFunctorK")
class autoFunctorK(autoDerivation: Boolean = true) extends StaticAnnotation {
  def macroTransform(annottees: Any*): Any = macro autoFunctorKMacros.newDef
}

private class autoFunctorKMacros(override val c: whitebox.Context) extends MacroUtils with CovariantKMethodsGenerator {
  import c.universe._

  private def generateFunctorKFor(algebraName: String)(algebraType: Tree, typeParams: Seq[TypeDef]) =
    typeClassInstance(
      TermName("functorKFor" + algebraName),
      typeParams,
      tq"_root_.cats.tagless.FunctorK[$algebraType]",
      q"_root_.cats.tagless.Derive.functorK[$algebraType]"
    )

  def instanceDef(algebra: AlgDefn.UnaryAlg): Tree =
    algebra.forVaryingEffectType(generateFunctorKFor(algebra.name))

  def instanceDefFullyRefined(algDefn: AlgDefn.UnaryAlg): Tree = {
    algDefn.forVaryingEffectTypeFullyRefined { (algebraType, tparams) =>
      val impl = Seq(
        generateFunctorKFor("FullyRefined" + algDefn.name)(
          algebraType,
          tparams
        ),
        generateAutoDerive(algDefn.fullyRefinedTypeSig)(
          algebraType,
          tparams
        )
      )
      q"object fullyRefined { ..$impl }"
    }
  }

  def newDef(annottees: c.Tree*): c.Tree =
    enrichAlgebra(annottees.toList)(algebra =>
      instanceDef(algebra) :: companionMapKDef(algebra) :: instanceDefFullyRefined(algebra) :: autoDerivationDef(
        algebra
      ) :: Nil
    )
}

private trait CovariantKMethodsGenerator { self: MacroUtils =>
  import c.universe._

  def companionMapKDef(algDefn: AlgDefn.UnaryAlg) = {
    val from                 = TermName("af")
    val F                    = createFreshTypeParam("F", 1)
    val G                    = createFreshTypeParam("G", 1)
    val algebraF             = algDefn.newTypeSig(F)
    val fullyRefinedAlgebraG = algDefn.dependentRefinedTypeSig(G, from)

    algDefn.forVaryingEffectType((algebraType, tparams) => q"""
      def mapK[$F, $G, ..$tparams]($from: $algebraF)(fk: _root_.cats.~>[..${tArgs(F, G)}]): $fullyRefinedAlgebraG =
        _root_.cats.tagless.FunctorK[$algebraType].mapK($from)(fk).asInstanceOf[$fullyRefinedAlgebraG]
    """)
  }

  def generateAutoDerive(newTypeSig: TypeDef => TypTree)(algebraType: Tree, tparams: Seq[TypeDef]) = {
    val F        = createFreshTypeParam("F", 1)
    val G        = createFreshTypeParam("G", 1)
    val algebraF = newTypeSig(F)
    val algebraG = newTypeSig(G)

    q"""
      object autoDerive {
        @SuppressWarnings(Array("org.wartremover.warts.ImplicitParameter"))
        implicit def fromFunctorK[$F, $G, ..$tparams](
          implicit fk: _root_.cats.~>[..${tArgs(F, G)}],
          FK: _root_.cats.tagless.FunctorK[$algebraType],
          af: $algebraF)
          : $algebraG = FK.mapK(af)(fk)
      }"""
  }

  def autoDerivationDef(algDefn: AlgDefn.UnaryAlg) =
    if (autoDerive) algDefn.forVaryingEffectType(generateAutoDerive(algDefn.newTypeSig)) else EmptyTree

}
//
//import scala.collection.mutable
//import scala.collection.mutable.ListBuffer
//import scala.reflect.macros.blackbox
////import scala.language.experimental.macros
////import scala.reflect.macros.blackbox.Context
//
//object TaglessWriterT {
//  def printf(format: String, params: Any*): Unit = macro Impl.printf_impl
//
//  private object Impl {
//    def printf_impl(c: blackbox.Context)(format: c.Expr[String], params: c.Expr[Any]*): c.Expr[Unit] = {
//      import c.universe._
//
//      val Literal(Constant(s_format: String)) = format.tree
//
//      val evals = ListBuffer[ValDef]()
//      def precompute(value: Tree, tpe: Type): Ident = {
//        val freshName = TermName(c.freshName("eval$"))
//        evals += ValDef(Modifiers(), freshName, TypeTree(tpe), value)
//        Ident(freshName)
//      }
//
//      val paramsStack = mutable.Stack[Tree]((params map (_.tree)): _*)
//      val refs = s_format.split("(?<=%[\\w%])|(?=%[\\w%])") map {
//        case "%d" => precompute(paramsStack.pop, typeOf[Int])
//        case "%s" => precompute(paramsStack.pop, typeOf[String])
//        case "%%" => Literal(Constant("%"))
//        case part => Literal(Constant(part))
//      }
//
//      val stats = evals ++ refs.map(ref => reify(print(c.Expr[Any](ref).splice)).tree)
//      c.Expr[Unit](Block(stats.toList, Literal(Constant(()))))
//
//
//      //    ???
//    }
//  }
//}
//
//
