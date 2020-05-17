package example

import cats._
import cats.implicits._

case class Call(methodName: String, argument: List[List[Any]])

trait Program[F[_]] {

  def foo(i: Int, j: Int): F[Int]

  def bar(s1: String, s2: String): F[Either[Throwable, String]]
}

object Program {
  @wrikito def `a*`[F[_]: Applicative](service: Service[F], barError: F[Either[Throwable, String]]): Program[F] = new Program[F] {
    override def foo(i: Int, j: Int): ({ type Z[YY] = F[YY] })#Z[Int] =
      Applicative[F].pure(i)

    override def bar(s1: String, s2: String): F[Either[Throwable, String]] = {
      val a: F[Either[Throwable, String]] = {
        try service.baz(s1.toInt).map(i => Right(i.toString))
        catch { case _: Throwable => barError }
      }
      a
    }
  }
}

trait Service[F[_]] {

  def baz(i: Int): F[Int]

}






object Service {

  @wrikito def impl[F[_]](implicit F: Applicative[F]): Service[F] = new Service[F] {
    override def baz(i: Int): F[Int] = F.map(F.pure(i * 10))(_ - 40)
  }

}

object Hello {

  def main(args: Array[String]): Unit = {
    println(Program.`a*_writerT`[Option](Service.impl_writerT[Option], Some(Left(new Exception("a")))).foo(3, 5))
  }

}
