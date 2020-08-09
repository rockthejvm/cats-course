package playground

import cats.Eval

object Playground {

  val meaningOfLife = Eval.later {
    println("Learning Cats: computing abstractions and the meaning of life...")
    42
  }

  def main(args: Array[String]): Unit = {
    println(meaningOfLife.value)
  }
}
