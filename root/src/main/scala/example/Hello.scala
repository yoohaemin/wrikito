package example

import cats.Applicative
import cats.implicits._


case class Call(methodName: String, argument: List[List[Any]])

trait Algebra[F[_]] {

  def foo(i: Int, j: Int): F[Int]

  def bar(s1: String, s2: String): F[Either[Throwable, String]]
}

object Algebra {

  @wrikito def `a*`[F[_]: Applicative]: Algebra[F] = new Algebra[F] {
    override def foo(i: Int, j: Int): ({ type Z[UUUU] = F[UUUU] })#Z[Int] = Applicative[F].pure(i + j)

    override def bar(s1: String, s2: String): F[Either[Throwable, String]] = {
      val a: F[Either[Throwable, String]] = Applicative[F].pure(Right(s1 + s2))
      a
    }
  }

//  def withWriterT[F[_]: _root_.cats.Applicative](underlying: Algebra[F]): Algebra[WriterT[F, List[Call], *]] =
//    new Algebra[WriterT[F, List[Call], *]] {
//
//      override def foo(i: Int): WriterT[F, List[Call], Int] = {
//        val x: WriterT[F, List[Call], Int] = WriterT.liftF( underlying.foo(i) )
//        x.tell(List(Call("foo", List(i))))
//      }
//
//      override def bar(s1: String, s2: String): WriterT[F, List[Call], Either[Throwable, String]] = {
//        val x: WriterT[F, List[Call], Either[Throwable, String]] = WriterT.liftF( underlying.bar(s1, s2) )
//
//        x.tell(List(Call("bar", List(s1, s2))))
//      }
//    }

//  object b extends Algebra[Option] {
//    override def foo: Option[Unit] = ???
//  }
}

object Hello {

//  val impl: Algebra[Try] = new Algebra[Try] {
//    override def foo: Try[Unit] = Try(())
//  }
//
//  val implOpt: Algebra[Option] = impl.mapK[Option](λ[Try ~> Option](_.toOption))
//
//  case class Call(methodName: String, arguments: List[Any])
//
//  def wrapCall[F[_]: Applicative]: F ~> WriterT[F, List[Call], *] =
//    new FunctionK[F, WriterT[F, List[Call], *]] {
//
//      override def apply[A](fa: F[A]): WriterT[F, List[Call], A] = {
//        type tpe = WriterT[F, List[Call], A]
//
//        val x: tpe = WriterT.liftF(fa)
//
//        val name: String = ""
//        val arguments: List[Any] = List.empty
//
//        x
//          .tell(Call(name, arguments) :: Nil)
//      }
//    }
//
//  def implWriterT[F[_]: Applicative](alg: Algebra[F]): Algebra[WriterT[F, List[Call], *]] =
//    alg.mapK[WriterT[F, List[Call], *]](wrapCall[F])

  def main(args: Array[String]): Unit = {
    println(Algebra.`a*_writerT`[Option].foo(3, 5))
  }

}
