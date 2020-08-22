package part5alien

import cats.Monoid

object InvariantFunctors {

  trait Crypto[A] { self =>
    def encrypt(value: A): String
    def decrypt(encrypted: String): A

    def imap[B](back: B => A, forth: A => B): Crypto[B] = new Crypto[B] {
      override def encrypt(value: B) = self.encrypt(back(value))
      override def decrypt(encrypted: String) = forth(self.decrypt(encrypted))
    }
  }

  def encrypt[A](value: A)(implicit crypto: Crypto[A]): String = crypto.encrypt(value)
  def decrypt[A](repr: String)(implicit crypto: Crypto[A]): A = crypto.decrypt(repr)

  implicit val caesarCypher: Crypto[String] = new Crypto[String] {
    override def encrypt(value: String) = value.map(c => (c + 2).toChar)
    override def decrypt(encrypted: String) = encrypted.map(c => (c - 2).toChar)
  }

  /*
    How can we support ints, doubles, Option[String]?
   */
  implicit val doubleCrypto: Crypto[Double] = caesarCypher.imap(_.toString, _.toDouble)

  // TODO 1 - support Option[String]
  implicit val optionStringCrypto: Crypto[Option[String]] = caesarCypher.imap(_.getOrElse(""), Option(_))

  // TODO 2 - if you have a Crypto[T] => Crypto[Option[T]] if you have a Monoid[T] in scope
  implicit def optionCrypto[T](implicit crypto: Crypto[T], monoid: Monoid[T]): Crypto[Option[T]] =
    crypto.imap(_.getOrElse(monoid.empty), Option(_))

  import cats.Invariant
  import cats.Show
  import cats.instances.string._ // Show[String]
  val showString = Show[String]
  val showOptionString: Show[Option[String]] = Invariant[Show].imap(showString)(Option(_))(_.getOrElse(""))

  import cats.syntax.invariant._
  val showOptionString2: Show[Option[String]] = showString.imap(Option(_))(_.getOrElse("")) // identical

  // TODO - what's the relationship?
  trait MyInvariant[W[_]] {
    def imap[A, B](wa: W[A])(forth: A => B)(back: B => A): W[B]
  }

  trait MyContravariant[W[_]] extends MyInvariant[W] {
    def contramap[A, B](wa: W[A])(back: B => A): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A) =
      contramap(wa)(back)
  }

  trait MyFunctor[W[_]] extends MyInvariant[W] { // "covariant" functor
    def map[A, B](wa: W[A])(forth: A => B): W[B]

    override def imap[A, B](wa: W[A])(forth: A => B)(back: B => A) =
      map(wa)(forth)
  }

  def main(args: Array[String]): Unit = {
    val encrypted = encrypt("Let's encrypt")
    val decrypted = decrypt[String](encrypted)

    println(encrypted)
    println(decrypted)
    println(encrypt(Math.PI))
    println(decrypt[Double](encrypt(Math.PI)))
    println(encrypt(Option("Let's encrypt")))
    println(decrypt[Option[String]](encrypted)) // Some("Let's encrypt")
    import cats.instances.double._
    println(encrypt(Option(Math.PI)))
    println(decrypt[Option[Double]](encrypt(Math.PI)))

  }
}
