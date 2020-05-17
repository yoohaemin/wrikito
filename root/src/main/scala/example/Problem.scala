package problem

import cats.Applicative
import cats.data.WriterT

case class Call(methodName: String, argument: List[List[Any]])

trait Program[F[_]] {

  def foo(i: Int): F[Int]
}

object Program {
  // Simple implementation
  def programImpl[F[_]: Applicative]: Program[F] = new Program[F] {
    override def foo(i: Int): F[Int] = Applicative[F].pure(i)
  }

  // Assumption: all public methods return F[xxx], so just rewrite mechanically
  def rewrittenProgram[F[_]: Applicative]: Program[WriterT[F, List[Call], *]] = new Program[WriterT[F, List[Call], *]] {
    override def foo(i: Int): WriterT[F, List[Call], Int] = {
      val initialResult: F[Int] = Applicative[F].pure(i)

      import cats.instances.list._
      val writerTApplied: WriterT[F, List[Call], Int] = WriterT.liftF[F, List[Call], Int](initialResult)
      writerTApplied.tell(List(Call("foo", List(List(i)))))
    }
  }

  // Here comes the problem
  trait Service[F[_]] {
    def bar(i: Int): F[Int]
  }

  // We want to rewrite `service: Service[F]` into `service: Service[WriterT[F, List[Call], *]`
  // However that breaks the above assumption
  def notSoEasy[F[_]: Applicative](service: Service[F]): Program[F] = new Program[F] {
    override def foo(i: Int): F[Int] =
      service.bar(i) //We no longer know if we can do `WriterT.liftF`
  }

  // We can just push typechecking into runtime
  def fragileSolution[F[_]: Applicative](service: Service[WriterT[F, List[Call], *]]): Program[WriterT[F, List[Call], *]] = new Program[WriterT[F, List[Call], *]] {
    override def foo(i: Int): WriterT[F, List[Call], Int] = {
      val result: Any = service.bar(i) // could be Applicative.pure[F]
      result match {
        // Probably WriterT case should come first
        case noNeed: WriterT[F, List[Call], Int] @unchecked => ??? // just do your stuff
        case needsLifting: F[Int] @unchecked => ??? // lift and do your stuff
      }
    }
  }

  // How about this?
  def evenHarder[F[_]: Applicative](service: Service[F]): Program[F] = new Program[F] {
    override def foo(i: Int): F[Int] = {
      val result: F[Int] = service.bar(i) //is F[Int] rewritable into WriterT[F, List[Call], Int]?
      result
    }
  }
}



trait Service[F[_]] {
  def bar(i: Int): F[Int]
}

