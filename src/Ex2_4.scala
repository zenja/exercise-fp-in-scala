/**
 * Implement uncurry, which reverses the transformation of curry. Note that since =>
 * associates to the right, A => (B => C) can be written as A => B => C.
 *
 * def uncurry[A,B,C](f: A => B => C): (A, B) => C
 */
object Ex2_4 {
  def main(args: Array[String]): Unit = {
    val f: (Int => Int => Int) = x => y => x + y
    val un = uncurry(f)
    println(s"1 + 2 = ${un(1, 2)}")
  }

  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }
}
