package ch2_getting_started

/**
 * Implement isSorted, which checks whether an Array[A] is sorted according to a
 * given comparison function:
 *
 * def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean
 */
object Ex2_2 {
  def main(args: Array[String]): Unit = {
    val arr1 = Array(1, 2, 3, 4, 5)
    val arr2 = Array(1, 2, 0, 4, 5)
    val arr3 = Array(1)
    val arr4: Array[Int] = Array()
    println(s"is [${arr1.mkString(",")}] sorted: " + isSorted(arr1, (x: Int, y: Int) => x < y))
    println(s"is [${arr2.mkString(",")}] sorted: " + isSorted(arr2, (x: Int, y: Int) => x < y))
    println(s"is [${arr3.mkString(",")}] sorted: " + isSorted(arr3, (x: Int, y: Int) => x < y))
    println(s"is [${arr4.mkString(",")}] sorted: " + isSorted(arr4, (x: Int, y: Int) => x < y))
  }

  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    def loop(n: Int): Boolean = {
      if (n >= as.length) true
      else if (!ordered(as(n-2), as(n-1))) false
      else loop(n+1)
    }

    loop(2)
  }
}
