package ch2_getting_started

/**
 * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
 *
 * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
 * previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
 * local tail-recursive function.
 *
 * def fib(n: Int): Int
 */
object Ex2_1 {
  def main(args: Array[String]): Unit = {
    for (i <- Range(0, 10)) {
      println(s"No.${i} fibonacci num: ${fib(i)}")
    }
  }

  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc1: Int, acc2: Int): Int = {
      if (n <= 0) acc1
      else go(n-1, acc2, acc1+acc2)
    }

    go(n, 0, 1)
  }

}
