package ch5_strictness_and_laziness

/**
 * Ex 5.1
 * Write a function to convert a Stream to a List,
 * which will force its evaluation and let you look at it in the REPL.
 * You can convert to the regular List type in the standard library.
 * You can place this and other functions that operate on a Stream inside the Stream trait.
 *
 * def toList: List[A]
 */
object Ex5_Stream {
  sealed trait Stream[+A] {
    def toListNotTailRecursive(): List[A] =
      this match {
        case Empty => List()
        case Cons(h, t) => h() :: t().toListNotTailRecursive
      }

    // tail recursive version from fpinscala Github
    def toList(): List[A] = {
      @annotation.tailrec
      def go(s: Stream[A], acc: List[A]): List[A] = s match {
        case Cons(h, t) => go(t(), h() :: acc)
        case _ => acc
      }

      go(this, List()).reverse
    }
  }

  case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]
  case object Empty extends Stream[Nothing]

  object Stream {
    def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
      lazy val head = hd
      lazy val tail = tl
      Cons(() => head, () => tail)
    }

    def empty[A]: Stream[A] = Empty

    def apply[A](as: A*): Stream[A] =
      if (as.isEmpty) empty else cons(as.head, apply(as.tail: _*))
  }

  def main(args: Array[String]): Unit = {
    val s: Stream[Int] = Stream.cons(0, Stream.cons(1, Stream.cons(2, Empty)))
    val es: Stream[Int] = Empty

    // Ex 5.1
    assert(s.toListNotTailRecursive == List(0, 1, 2), "toListNotTailRecursive test case 1")
    assert(es.toListNotTailRecursive == List(), "toListNotTailRecursive test case 2")
    assert(s.toList == List(0, 1, 2), "toList test case 1")
    assert(es.toList == List(), "toList test case 2")

    println("All tests finished.")
  }

}
