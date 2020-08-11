package part1intro

object CatsIntro {

  // Eq
  // val aComparison = 2 == "a string" -- wrong, will trigger a compiler warning, will always return false

  // part 1 - type class import
  import cats.Eq

  // part 2 - import TC instances for the types you need
  import cats.instances.int._

  // part 3 - use the TC API
  val intEquality = Eq[Int]
  val aTypeSafeComparison = intEquality.eqv(2, 3) // false
  //  val anUnsafeComparison = intEquality.eqv(2, "a string") -- doesn't compile!

  // part 4 - use extension methods (if applicable)
  import cats.syntax.eq._
  val anotherTypeSafeComp = 2 === 3 // false
  val neqComparison = 2 =!= 3 // true
  // val invalidComparison = 2 === "a String" -- doesn't compile
  // extension methods are only visible in the presence of the right TC instance

  // part 5 - extending the TC operations to composite types, e.g. lists
  import cats.instances.list._ // we bring Eq[List[Int]] in scope
  val aListComparison = List(2) === List(3) // returns false

  // part 6 - create a TC instance for a custom type
  case class ToyCar(model: String, price: Double)
  implicit val toyCarEq: Eq[ToyCar] = Eq.instance[ToyCar] { (car1, car2) =>
    car1.price == car2.price
  }

  val compareTwoToyCars = ToyCar("Ferrari", 29.99) === ToyCar("Lamborghini", 29.99) // true

}
