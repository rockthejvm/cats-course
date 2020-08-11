package part1intro

object Implicits {

  // implicit classes
  case class Person(name: String) {
    def greet: String = s"Hi, my name is $name!"
  }

  implicit class ImpersonableString(name: String) {
    def greet: String = Person(name).greet
  }
  val greeting = "Peter".greet // new ImpersonableString("Peter").greet

  // importing implicit conversions in scope
  import scala.concurrent.duration._
  val oneSec = 1.second

  // implicit arguments and values
  def increment(x: Int)(implicit amount: Int) = x + amount
  implicit val defaultAmount = 10
  val incremented2 = increment(2) // implicit argument 10 is passed by the compiler

  def multiply(x: Int)(implicit times: Int) = x * times
  val times2 = multiply(2)

  // more complex example
  trait JSONSerializer[T] {
    def toJson(value: T): String
  }

  def listToJson[T](list: List[T])(implicit serializer: JSONSerializer[T]): String =
    list.map(value => serializer.toJson(value)).mkString("[", ",", "]")

  implicit val personSerializer: JSONSerializer[Person] = new JSONSerializer[Person] {
    override def toJson(person: Person) =
      s"""
         |{"name" : "${person.name}"}
         |""".stripMargin
  }

  val personsJson = listToJson(List(Person("Alice"), Person("Bob")))
  // implicit argument is used to PROVE THE EXISTENCE of a type

  // implicit methods
  implicit def oneArgCaseClassSerializer[T <: Product]: JSONSerializer[T] = new JSONSerializer[T] {
    override def toJson(value: T) =
      s"""
         |{"${value.productElementName(0)}" : "${value.productElement(0)}"}
         |""".stripMargin.trim
  }

  case class Cat(catName: String)
  val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))
  // in the background: val catsToJson = listToJson(List(Cat("Tom"), Cat("Garfield")))(oneArgCaseClassSerializer[Cat])
  // implicit methods are used to PROVE THE EXISTENCE of a type
  // can be used for implicit conversions (DISCOURAGED)


  def main(args: Array[String]): Unit = {

  }
}
