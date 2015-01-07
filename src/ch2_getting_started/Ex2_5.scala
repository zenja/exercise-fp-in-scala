package ch2_getting_started

/**
 * Implement the higher-order function that composes two functions.
 *
 * def compose[A,B,C](f: B => C, g: A => B): A => C
 */
object Ex2_5 {
  def main(args: Array[String]): Unit = {
    val f1: (Int => Int) = _ + 1
    val f2: (Int => Int) = _ + 2
    val com = compose(f1, f2)
    println(s"10 + 1 + 2 = ${com(10)}")

  }

  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}
