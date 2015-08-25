package ch10_monoids

/**
 * Ex 10.1
 * Give Monoid instances for integer addition and multiplication as well as the Boolean operators.
 *
 * val intAddition: Monoid[Int]
 * val intMultiplication: Monoid[Int]
 * val booleanOr: Monoid[Boolean] val booleanAnd: Monoid[Boolean]
 *
 * Ex 10.2
 * Give a Monoid instance for combining Option values.
 *
 * def optionMonoid[A]: Monoid[Option[A]]
 *
 * Ex 10.3
 * A function having the same argument and return type is sometimes called an endofunction.
 * Write a monoid for endofunctions.
 *
 * def endoMonoid[A]: Monoid[A => A]
 *
 * Ex 10.5
 * Implement foldMap.
 *
 * def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B
 *
 * Ex 10.6
 * Hard: The foldMap function can be implemented using either foldLeft or foldRight.
 * But you can also write foldLeft and foldRight using foldMap! Try it.
 *
 * Ex 10.7
 * Implement a foldMap for IndexedSeq.
 * Your implementation should use the strategy of splitting the sequence in two,
 * recursively processing each half, and then adding the answers together with the monoid.
 *
 * def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B
 *
 * Ex 10.9
 * Hard: Use foldMap to detect whether a given IndexedSeq[Int] is ordered.
 * You’ll need to come up with a creative Monoid.
 */
object Ex10 {
  trait Monoid[A] {
    def op(a1: A, a2: A): A
    def zero: A
  }

  val stringMonoid = new Monoid[String] {
    override def op(a1: String, a2: String) = a1 + a2
    override def zero = ""
  }

  /// Ex 10.1

  val intAddition = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 + a2
    override def zero = 0
  }

  val intMultiplication = new Monoid[Int] {
    override def op(a1: Int, a2: Int): Int = a1 * a2
    override def zero = 1
  }

  val booleanOr = new Monoid[Boolean] {
    override def op(a1: Boolean, a2: Boolean): Boolean = a1 || a2
    override def zero = false
  }

  /// Ex 10.2

  def optionMonoid[A]: Monoid[Option[A]] = new Monoid[Option[A]] {
    override def op(a1: Option[A], a2: Option[A]): Option[A] = a1.orElse(a2)

    override def zero: Option[A] = None
  }

  /// Ex 10.3 TODO not sure answer is right

  def endoMonoid[A]: Monoid[A => A] = new Monoid[A => A] {
    override def op(f1: (A) => A, f2: (A) => A): (A) => A = (a: A) => f1(f2(a))

    override def zero: (A) => A = identity[A]
  }

  /// Ex 10.5

  def foldMap[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldLeft(m.zero)(m.op)
  }

  def foldMap2[A,B](as: List[A], m: Monoid[B])(f: A => B): B = {
    as.map(f).foldRight(m.zero)(m.op)
  }

  /// Ex 10.6 TODO not sure if correct

  def foldRight[A](z: A)(as: List[A], f: (A, A) => A): A = {
    val monoid = new Monoid[A] {
      override def op(a1: A, a2: A): A = f(a1, a2)
      override def zero: A = z
    }
    foldMap[A, A](as, monoid)(identity[A])
  }

  def foldLeft[A](z: A)(as: List[A], f: (A, A) => A): A = {
    foldRight[A](z)(as, f)
  }

  /// Ex 10.7 TODO not sure

  def foldMapV[A,B](v: IndexedSeq[A], m: Monoid[B])(f: A => B): B = {
    if (v.size <= 2) {
      foldMap[A,B](v.toList, m)(f)
    } else {
      val firstPart: B = foldMapV[A,B](v.slice(0, v.size/2), m)(f)
      val secondPart: B = foldMapV[A,B](v.slice(v.size/2, v.size), m)(f)
      m.op(firstPart, secondPart)
    }
  }

  // Ex 10.9
  def ifSortedAsc[A: Ordering](as: List[A], minA: A): Boolean = {
    val order = implicitly[Ordering[A]]

    val monoid = new Monoid[(A, Boolean)] {
      override def op(a1: (A, Boolean), a2: (A, Boolean)): (A, Boolean) = {
        val larger = if (a1._1 == minA || order.lt(a1._1, a2._1)) a2._1 else a1._1
        val isAsc = a1._2 && a2._2 && order.lteq(a1._1, a2._1)
        (larger, isAsc)
      }

      override def zero: (A, Boolean) = (minA, true)
    }

    foldMap[A, (A, Boolean)](as, monoid)((a: A) => (a, true))._2
  }

  /// Tests

  def main(args: Array[String]): Unit = {
    // Ex 10.1
    assert(intAddition.op(1, 2) == 3)
    assert(intAddition.op(9, intAddition.zero) == 9)
    assert(intMultiplication.op(1, 2) == 2)
    assert(intMultiplication.op(9, intMultiplication.zero) == 9)
    assert(booleanOr.op(false, true) == true)
    assert(booleanOr.op(true, false) == true)
    assert(booleanOr.op(true, true) == true)
    assert(booleanOr.op(false, false) == false)
    assert(booleanOr.op(true, booleanOr.zero) == true)
    assert(booleanOr.op(false, booleanOr.zero) == false)

    // Ex 10.2
    val intOptionMonoid = optionMonoid[Int]
    assert(intOptionMonoid.op(Some(1), Some(2)) == Some(1))
    assert(intOptionMonoid.op(Some(1), None) == Some(1))
    assert(intOptionMonoid.op(None, Some(2)) == Some(2))
    assert(intOptionMonoid.op(None, None) == None)
    assert(intOptionMonoid.op(None, intOptionMonoid.zero) == None)
    assert(intOptionMonoid.op(Some(1), intOptionMonoid.zero) == Some(1))

    // Ex 10.3
    val intEndoMonoid = endoMonoid[Int]
    assert(intEndoMonoid.op((a: Int) => a + 1, (a: Int) => a + 2)(10) == 13)
    assert(intEndoMonoid.op((a: Int) => a + 1, intEndoMonoid.zero)(10) == 11)

    // Ex 10.5
    assert(foldMap[Int, String](List(1,2,3), stringMonoid)((a: Int) => a.toString) == "123")
    assert(foldMap[Int, String](List(1,2,3), stringMonoid)((a: Int) => a.toString) ==
      foldMap2[Int, String](List(1,2,3), stringMonoid)((a: Int) => a.toString))

    // Ex 10.6
    assert(foldRight[Int](0)(List(1,2,3), _ + _) == 6)

    // Ex 10.7
    assert(foldMap[Int, String](List(1,2,3), stringMonoid)((a: Int) => a.toString) ==
      foldMapV[Int, String](Array(1,2,3), stringMonoid)((a: Int) => a.toString))

    // Ex 10.9
    assert(ifSortedAsc[Int](List(1,2,3), -1))
    assert(!ifSortedAsc[Int](List(1,3,2), -1))
    assert(!ifSortedAsc[Int](List(3,2,1), -1))
  }
}
