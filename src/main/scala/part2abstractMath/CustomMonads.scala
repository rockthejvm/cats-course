package part2abstractMath

import scala.annotation.tailrec

object CustomMonads {

  import cats.Monad
  implicit object OptionMonad extends Monad[Option] {
    override def pure[A](x: A): Option[A] = Option(x)
    override def flatMap[A, B](fa: Option[A])(f: A => Option[B]): Option[B] = fa.flatMap(f)

    // tailrecM does NOT stack-overflow
    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Option[Either[A, B]]): Option[B] = f(a) match {
      case None => None
      case Some(Left(v)) => tailRecM(v)(f)
      case Some(Right(b)) => Some(b)
    }
  }

  // TODO 1: define a monad for the identity type
  type Identity[T] = T
  val aNumber: Identity[Int] = 42
  implicit object IdentityMonad extends Monad[Identity] {
    def pure[A](x: A): Identity[A] = x
    override def flatMap[A, B](a: Identity[A])(f: A => Identity[B]): Identity[B] = f(a)

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[Either[A, B]]): Identity[B] = f(a) match {
      case Left(v) => tailRecM(v)(f)
      case Right(b) => b
    }
  }

  // harder example
  sealed trait Tree[+A]
  final case class Leaf[+A](value: A) extends Tree[A]
  final case class Branch[+A](left: Tree[A], right: Tree[A]) extends Tree[A]

  // TODO 2: define a monad for this Tree
  // tailrecM tailrec is difficult
  implicit object TreeMonad extends Monad[Tree] {
    def pure[A](x: A): Tree[A] = Leaf(x)
    def flatMap[A, B](ta: Tree[A])(f: A => Tree[B]): Tree[B] = ta match {
      case Leaf(v) => f(v)
      case Branch(left, right) => Branch(flatMap(left)(f), flatMap(right)(f))
    }

    override def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] = {
      def stackRec(t: Tree[Either[A, B]]): Tree[B] = t match {
        case Leaf(Left(v)) => stackRec(f(v))
        case Leaf(Right(b)) => Leaf(b)
        case Branch(left, right) => Branch(stackRec(left), stackRec(right))
      }

      /*
            _____1_____
         __2__       __3__
        /     \     /     \
       L1     R2   R3     R4

        tr([1], [], []) =
        tr([3, 2, 1], [1], []) =
        tr([R4, R3, 3, 2, 1], [1, 3], []) =
        tr([R3, 3, 2, 1], [1, 3], [B4]) =
        tr([3, 2, 1], [1, 3], [B3, B4]) =
        tr([2, 1], [1, 3], [B34]) =
        tr([R2, L1, 2, 1], [1,2,3], [B34]) =
        tr([L1, 2, 1], [1,2,3], [B2, B34]) =
        tr([R1, 2, 1], [1,2,3], [B2, B34]) =
        tr([2,1], [1,2,3], [B1, B2, B34]) =
        tr([1], [1,2,3], [B12, B34]) =
        tr([], [1,2,3], [B1234]) =
        B1234

       */
      def tailRec(todo: List[Tree[Either[A, B]]], expanded: Set[Tree[Either[A, B]]], done: List[Tree[B]]): Tree[B] =
        if (todo.isEmpty) done.head
        else todo.head match {
          case Leaf(Left(v)) => tailRec(f(v) :: todo.tail, expanded, done)
          case Leaf(Right(b)) => tailRec(todo.tail, expanded, Leaf(b) :: done)
          case node @ Branch(left, right) =>
            if (!expanded.contains(node)) {
              tailRec(right :: left :: todo, expanded + node, done)
            } else {
              val newLeft = done.head
              val newRight = done.tail.head
              val newBranch = Branch(newLeft, newRight)
              tailRec(todo.tail, expanded, newBranch :: done.drop(2))
            }
        }

      tailRec(List(f(a)), Set(), List())
    }
  }

  def main(args: Array[String]): Unit = {
    val tree: Tree[Int] = Branch(Leaf(10), Leaf(20))
    val changedTree = TreeMonad.flatMap(tree)(v => Branch(Leaf(v + 1), Leaf(v + 2)))
    println(changedTree)

  }
}
