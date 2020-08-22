package part5alien

import cats.kernel.Monoid

object ContravariantFunctors {

  trait Format[T] { self => // contravariant type classes
    def format(value: T): String

    def contramap[A](func: A => T): Format[A] = new Format[A] {
      override def format(value: A) = self.format(func(value))
    }
  }

  def format[A](value: A)(implicit f: Format[A]) = f.format(value)

  implicit object StringFormat extends Format[String] {
    override def format(value: String) = "\"" + value + "\""
  }

  implicit object IntFormat extends Format[Int] {
    override def format(value: Int) = value.toString
  }

  implicit object BooleanFormat extends Format[Boolean] {
    override def format(value: Boolean) = if (value) "Y" else "N"
  }

  // problem: given Format[MyType], can we have a Format[Option[MyType]]?
  implicit def getOptionFormat[T](implicit f: Format[T], m: Monoid[T]): Format[Option[T]] =
    f.contramap[Option[T]](_.getOrElse(m.empty))

  /*
    IntFormat
    fo: Format[Option[Int]] = IntFormat.contramap[Option[Int]])(_.get) // first get
    fo2: Format[Option[Option[Int]]] = fo.contramap[Option[Option[Int]]](_.get) // second get

    fo2 = IntFormat
      .contramap[Option[Int]])(_.get) // first get
      .contramap[Option[Option[Int]]](_.get) // second get

    fo2.format(Option(Option(42))) =
      fo1.format(secondGet(Option(Option(42))) =
      IntFormat.format(firstGet(secondGet(Option(Option(42))))

    order = REVERSE from the written order
    - second get
    - first get
    - format of Int

    Map applies transformations in sequence
    Contramap applies transformations in REVERSE sequence
   */

  import cats.Contravariant
  import cats.Show
  import cats.instances.int._ // implicit Show[Int]
  val showInts = Show[Int]
  val showOption: Show[Option[Int]] = Contravariant[Show].contramap(showInts)(_.getOrElse(0))

  import cats.syntax.contravariant._
  val showOptionsShorter: Show[Option[Int]] = showInts.contramap(_.getOrElse(0))

  def main(args: Array[String]): Unit = {
    println(format("Nothing weird so far"))
    println(format(42))
    println(format(true))
    println(format(Option(42)))
    import cats.instances.option._
    println(format(Option(Option(42))))
  }
}
