package ch5_strictness_and_laziness

/**
 * Ex 5.1
 * Write a function to convert a Stream to a List,
 * which will force its evaluation and let you look at it in the REPL.
 * You can convert to the regular List type in the standard library.
 * You can place this and other functions that operate on a Stream inside the Stream trait.
 *
 * def toList: List[A]
 *
 * Ex 5.2
 * Write the function take(n) for returning the first n elements of a Stream,
 * and drop(n) for skipping the first n elements of a Stream.
 *
 * Ex 5.3
 * Write the function takeWhile for returning all starting elements of a Stream that
 * match the given predicate.
 *
 * def takeWhile(p: A => Boolean): Stream[A]
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

    def take(n: Int): Stream[A] =
      /* My ugly solution:
      if (n <= 0)
        return Empty
      else
        return this match {
          case Cons(h, t) => Stream.cons(h(), t().take(n - 1))
          case _ => Empty
        }
      */
      // solution from fpinscala Github
      this match {
        case Cons(h, t) if n > 1 => Stream.cons(h(), t().take(n - 1))
        case Cons(h, _) if n == 1 => Stream.cons(h(), Empty)
        case _ => Empty
      }

    def takeWhile(p: A => Boolean): Stream[A] =
      this match {
        case Cons(h, t) if p(h()) => Stream.cons(h(), t() takeWhile p)
        case _ => Empty
      }

    def drop(n: Int): Stream[A] =
      this match {
        case Cons(_, t) if n >= 1 => t().drop(n - 1)
        case _ => this
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

    // Ex 5.2
    assert(s.take(2).toList == List(0, 1), "take test case 1")
    assert(s.take(-1).toList == List(), "take test case 2")
    assert(es.take(1).toList == List(), "take test case 3")

    assert(s.drop(1).toList == List(1, 2), "drop test case 1")
    assert(s.drop(100).toList == List(), "drop test case 1")
    assert(s.drop(-1).toList == s.toList, "drop test case 1")

    // Ex 5.3
    assert(s.takeWhile(_ => false).toList == List(), "takeWhile test case 1")
    assert(s.takeWhile(_ < 2).toList == List(0, 1), "takeWhile test case 2")
    assert(s.takeWhile(_ < 10).toList == List(0, 1, 2), "takeWhile test case 3")

    println("All tests finished.")
  }

}