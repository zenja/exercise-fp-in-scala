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
 *
 * Ex 5.4
 * Implement forAll, which checks that all elements in the Stream match a given predicate.
 * Your implementation should terminate the traversal as soon as it encounters a
 * non-matching value.
 *
 * def forAll(p: A => Boolean): Boolean
 *
 * Ex 5.5
 * Use foldRight to implement takeWhile.
 *
 * Ex 5.6
 * Hard: Implement headOption using foldRight.
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

    def headOption: Option[A] = this match {
      case Empty => None
      case Cons(h, t) => Some(h())
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

    def foldRight[B](z: => B)(f: (A, => B) => B): B =
      this match {
        case Cons(h,t) => f(h(), t().foldRight(z)(f))
        case _ => z
      }

    def exists(p: A => Boolean): Boolean =
      this match {
        case Cons(h, t) => p(h()) || t().exists(p)
        case _ => false
      }

    // implement exists with foldRight
    def existsFR(p: A => Boolean): Boolean =
      foldRight(false)((a, b) => p(a) || b)

    // Since `&&` is non-strict in its second argument,
    // this terminates the traversal as soon as a nonmatching element is found.
    def forAll(p: A => Boolean): Boolean =
      foldRight(true)((a, b) => p(a) && b)

    def takeWhileFR(p: A => Boolean): Stream[A] =
      foldRight(Stream.empty[A])((h, t) =>
        if (p(h)) Stream.cons(h, t)
        else Stream.empty[A])

    def headOptionFR(): Option[A] = {
      /* my answer:
      val none: Option[A] = None
      foldRight(none)((a, o) => Some(a))
      */
      foldRight(None: Option[A])((a, _) => Some(a))
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
    assert(s.drop(100).toList == List(), "drop test case 2")
    assert(s.drop(-1).toList == s.toList, "drop test case 3")

    // Ex 5.3
    assert(s.takeWhile(_ => false).toList == List(), "takeWhile test case 1")
    assert(s.takeWhile(_ < 2).toList == List(0, 1), "takeWhile test case 2")
    assert(s.takeWhile(_ < 10).toList == List(0, 1, 2), "takeWhile test case 3")

    // Ex 5.4
    assert(s.forAll(_ < 10) == true, "forAll test case 1")
    assert(s.forAll(_ < 2) == false, "forAll test case 2")
    assert(es.forAll(_ == 999) == true, "forAll test case 3")

    // Ex 5.5
    assert(s.takeWhileFR(_ < 10).toList == List(0, 1, 2), "takeWhileFR test case 1")
    assert(s.takeWhileFR(_ < 0).toList == List(), "takeWhileFR test case 2")
    assert(s.takeWhileFR(_ < 2).toList == List(0, 1), "takeWhileFR test case 3")

    // Ex 5.6
    assert(s.headOptionFR() == Option(0), "headOptionFR test case 1")
    assert(es.headOptionFR() == None, "headOptionFR test case 2")

    println("All tests finished.")
  }

}
