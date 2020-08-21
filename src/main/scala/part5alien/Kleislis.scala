package part5alien

import cats.data.Reader

object Kleislis {

  val func1: Int => Option[String] = x => if (x % 2 == 0) Some(s"$x is even") else None
  val func2: Int => Option[Int] = x => Some(x * 3)

  // val func3 = func2 andThen func1

  val plainFunc1: Int => String = x => if (x % 2 == 0) s"$x is even" else "fail"
  val plainFunc2: Int => Int = x => x * 3
  val plainFunc3: Int => String = plainFunc2 andThen plainFunc1

  import cats.data.Kleisli
  import cats.instances.option._ // FlatMap[Option]
  val func1K: Kleisli[Option, Int, String] = Kleisli(func1)
  val func2K: Kleisli[Option, Int, Int] = Kleisli(func2)
  val func3K: Kleisli[Option, Int, String] = func2K andThen func1K

  // convenience
  val multiply = func2K.map(_ * 2) // x => Option(...).map(_ * 2)
  val chain = func2K.flatMap(x => func1K)

  // TODO - ring a bell?
  import cats.Id
  type InterestingKleisli[A, B] = Kleisli[Id, A, B] // wrapper over A => Id[B]
  // InterestingKleisli == Reader!
  // hint
  val times2 = Reader[Int, Int](x => x * 2)
  val plus4 = Reader[Int, Int](y => y + 4)
  val composed = times2.flatMap(t2 => plus4.map(p4 => t2 + p4))
  val composedFor = for {
    t2 <- times2
    p4 <- plus4
  } yield t2 + p4

  def main(args: Array[String]): Unit = {
    println(composedFor(3)) // 13
  }
}
