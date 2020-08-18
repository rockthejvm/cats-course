package part4typeclasses

import java.util.concurrent.Executors



import scala.concurrent.{ExecutionContext, Future}

object Semigroupals {

  trait MySemigroupal[F[_]] {
    def product[A, B](fa: F[A], fb: F[B]): F[(A, B)]
  }

  import cats.Semigroupal
  import cats.instances.option._ // implicit Semigroupal[Option]
  val optionSemigroupal = Semigroupal[Option]
  val aTupledOption = optionSemigroupal.product(Some(123), Some("a string")) // Some((123, "a string"))
  val aNoneTupled = optionSemigroupal.product(Some(123), None) // None

  import cats.instances.future._ // implicit Semigroupal[Future]
  implicit val ec: ExecutionContext = ExecutionContext.fromExecutorService(Executors.newFixedThreadPool(8))
  val aTupledFuture = Semigroupal[Future].product(Future("the meaning of life"), Future(42)) // Future(("the meaning of life", 42))

  import cats.instances.list._ // Monad[List]
  val aTupledList = Semigroupal[List].product(List(1, 2), List("a", "b"))

  // TODO: implement product with monads
  import cats.Monad
  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap
  def productWithMonads[F[_], A, B](fa: F[A], fb: F[B])(implicit monad: Monad[F]): F[(A, B)] =
    for {
      a <- fa
      b <- fb
    } yield (a, b)

  // MONADS EXTEND SEMIGROUPALS

  // example: Validated
  import cats.data.Validated
  type ErrorsOr[T] = Validated[List[String], T]
  val validatedSemigroupal = Semigroupal[ErrorsOr] // requires the implicit Semigroup[List[_]]
  val invalidsCombination = validatedSemigroupal.product(
    Validated.invalid(List("Something wrong", "something else wrong")),
    Validated.invalid(List("This can't be right"))
  )

  type EitherErrorsOr[T] = Either[List[String], T]
  import cats.instances.either._ // implicit Monad[Either]
  val eitherSemigroupal = Semigroupal[EitherErrorsOr]
  val eitherCombination = eitherSemigroupal.product( // in terms of map/flatMap
    Left(List("Something wrong", "something else wrong")),
    Left(List("This can't be right"))
  )

  // Associativity: m.flatMap(f).flatMap(g) == m.flatMap(x => f(x).flatMap(g))

  // TODO 2: define a Semigroupal[List] which does a zip
  val zipListSemigroupal: Semigroupal[List] = new Semigroupal[List] {
    override def product[A, B](listA: List[A], listB: List[B]) = listA.zip(listB)
  }

  def main(args: Array[String]): Unit = {
    println(invalidsCombination)
    println(eitherCombination)
    println(zipListSemigroupal.product(List(1, 2), List("a", "b")))
  }
}
