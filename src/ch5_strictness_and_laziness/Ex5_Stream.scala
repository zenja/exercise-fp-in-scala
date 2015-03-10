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
 *
 * Ex 5.7
 * Implement map, filter, append, and flatMap using foldRight.
 * The append method should be non-strict in its argument.
 *
 * Ex 5.8
 * Generalize ones slightly to the function constant,
 * which returns an infinite Stream of a given value.
 *
 * def constant[A](a: A): Stream[A]
 *
 * Ex 5.9
 * Write a function that generates an infinite stream of integers,
 * starting from n, then n + 1, n + 2, and so on.
 *
 * def from(n: Int): Stream[Int]
 *
 * Ex 5.10
 * Write a function fibs that generates the infinite stream of Fibonacci numbers:
 * 0, 1, 1, 2, 3, 5, 8, and so on.
 *
 * Ex 5.11
 * Write a more general stream-building function called unfold.
 * It takes an initial state, and a function for producing
 * both the next state and the next value in the generated stream.
 *
 * def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A]
 *
 * Ex 5.12
 * Write fibs, from, constant, and ones in terms of unfold.
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

    def map[B](f: A => B): Stream[B] =
      foldRight(Stream.empty[B])((a, bs) => Stream.cons(f(a), bs))

    def filter(p: A => Boolean): Stream[A] =
      // my wrong answer:
      // foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t.filter(p)) else t.filter(p))
      foldRight(Stream.empty[A])((h, t) => if (p(h)) Stream.cons(h, t) else t)

    // append to the end of the stream
    def append[B>:A](s: => Stream[B]): Stream[B] =
      foldRight(s)((h, t) => Stream.cons(h, t))

    def flatMap[B](f: A => Stream[B]): Stream[B] =
      foldRight(Stream.empty[B])((h, t) => f(h) append t)
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

    def constant[A](a: A): Stream[A] = {
      // note that "lazy" must be added or it won't compile for the reason:
      // "forward reference extends over definition of value s"
      lazy val s: Stream[A] = Stream.cons(a, s)
      s
    }

    def fibs(): Stream[Int] = {
      def go(a: Int, b: Int): Stream[Int] =
        Stream.cons(a, go(b, a + b))
      go(0, 1)
    }

    def from(n: Int): Stream[Int] =
      Stream.cons(n, from(n + 1))

    def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
      f(z) match {
        case None => Empty
        case Some((a, s)) => Stream.cons(a, unfold(s)(f))
      }

    def fibsViaUnfold(): Stream[Int] =
      unfold((0, 1)){case (a,b) => Some((a, (b, a+b)))}

    def fromViaUnfold(n: Int): Stream[Int] =
      unfold(n)(n => Some(n, n+1))

    def constantViaUnfold[A](a: A): Stream[A] =
      unfold(a)(_ => Some(a, a))

    def onesViaUnfold(): Stream[Int] =
      unfold(1)(_ => Some(1, 1))
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

    // Ex 5.7
    assert(s.map(_ * 2).toList == List(0, 2, 4), "map test case 1")
    assert(es.map(_ * 2).toList == List(), "map test case 2")

    assert(s.filter(_ < 10).toList == List(0, 1, 2), "filter test case 1")
    assert(s.filter(_ < 2).toList == List(0, 1), "filter test case 2")
    assert(es.filter(_ < 100).toList == List(), "filter test case 3")

    assert(s.append(s.take(2)).toList == List(0, 1, 2, 0, 1), "append test case 1")
    assert(es.append(s.take(2)).toList == List(0, 1), "append test case 2")
    assert(s.append(es).toList == List(0, 1, 2), "append test case 3")

    assert(s.flatMap(x => Stream.cons(x+1, Stream.cons(x+2, Empty))).toList == List(1, 2, 2, 3, 3, 4), "flatMap test case 1")
    assert(es.flatMap(x => Stream.cons(x+1, Stream.cons(x+2, Empty))).toList == List(), "flatMap test case 2")

    // Ex 5.8
    assert(Stream.constant(9).take(3).toList == List(9, 9, 9), "constant test case 1")

    // Ex 5.9
    assert(Stream.from(5).take(3).toList == List(5, 6, 7), "from test case 1")

    // Ex 5.10
    assert(Stream.fibs().take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13), "fibs test case 1")

    // Ex 5.11
    // check by Ex 5.12

    // Ex 5.12
    assert(Stream.fibsViaUnfold().take(8).toList == List(0, 1, 1, 2, 3, 5, 8, 13), "fibsViaUnfold test case 1")

    assert(Stream.fromViaUnfold(5).take(3).toList == List(5, 6, 7), "fromViaUnfold test case 1")

    assert(Stream.constantViaUnfold(9).take(3).toList == List(9, 9, 9), "constantViaUnfold test case 1")

    assert(Stream.onesViaUnfold().take(5).toList == List(1, 1, 1, 1, 1), "onesViaUnfold test case 1")

    println("All tests finished.")
  }

}
