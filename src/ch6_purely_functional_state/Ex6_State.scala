package ch6_purely_functional_state

/**
 * Ex 6.1
 * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
 * Make sure to handle the corner case when nextInt returns Int.MinValue,
 * which doesn’t have a non-negative counterpart.
 *
 * def nonNegativeInt(rng: RNG): (Int, RNG)
 *
 * Ex 6.2
 * Write a function to generate a Double between 0 and 1, not including 1. Note: You can
 * use Int.MaxValue to obtain the maximum positive integer value, and you can use
 * x.toDouble to convert an x: Int to a Double.
 *
 * def double(rng: RNG): (Double, RNG)
 *
 * Ex 6.3
 * Write functions to generate an (Int, Double) pair, a (Double, Int) pair, and a
 * (Double, Double, Double) 3-tuple. You should be able to reuse the functions you’ve
 * already written.
 *
 * def intDouble(rng: RNG): ((Int,Double), RNG)
 * def doubleInt(rng: RNG): ((Double,Int), RNG)
 * def double3(rng: RNG): ((Double,Double,Double), RNG)
 *
 * Ex 6.4
 * Write a function to generate a list of random integers.
 *
 * def ints(count: Int)(rng: RNG): (List[Int], RNG)
 *
 * Ex 6.5
 * Use map to reimplement double in a more elegant way. See exercise 6.2.
 *
 * Ex 6.6
 * Write the implementation of map2 based on the following signature. This function
 * takes two actions, ra and rb, and a function f for combining their results, and returns
 * a new action that combines them:
 *
 * def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C]
 *
 * Ex 6.7
 * Hard: If you can combine two RNG transitions, you should be able to combine a whole
 * list of them. Implement sequence for combining a List of transitions into a single
 * transition. Use it to reimplement the ints function you wrote before. For the latter,
 * you can use the standard library function List.fill(n)(x) to make a list with x
 * repeated n times.
 *
 * def sequence[A](fs: List[Rand[A]]): Rand[List[A]]
 *
 * Ex 6.8
 * Implement flatMap, and then use it to implement nonNegativeLessThan.
 *
 * def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B]
 *
 * Ex 6.9
 * Reimplement map and map2 in terms of flatMap.
 * The fact that this is possible is what we’re referring to
 * when we say that flatMap is more powerful than map and map2.
 *
 * Ex 6.10
 * Generalize the functions unit, map, map2, flatMap, and sequence.
 * Add them as methods on the State case class where possible.
 * Otherwise you should put them in a State companion object.
 */
object Ex6_State {
  trait RNG {
    def nextInt: (Int, RNG)
  }

  type Rand[+A] = RNG => (A, RNG)

  case class SimpleRNG(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL
      val nextRNG = SimpleRNG(newSeed)
      val n = (newSeed >>> 16).toInt
      (n, nextRNG)
    }
  }

  object RNG {
    def nonNegativeInt(rng: RNG): (Int, RNG) = {
      val (i, r) = rng.nextInt
      (if (i < 0) -(i + 1) else i, r)
    }

    def double(rng: RNG): (Double, RNG) = {
      val (i, r) = nonNegativeInt(rng)
      (i/(Int.MaxValue.toDouble + 1), r)
    }

    def intDouble(rng: RNG): ((Int,Double), RNG) = {
      val (i, r1) = rng.nextInt
      val (d, r2) = double(r1)
      ((i, d), r2)
    }

    def doubleInt(rng: RNG): ((Double,Int), RNG) = {
      val ((i, d), r) = intDouble(rng)
      ((d, i), r)
    }

    def double3(rng: RNG): ((Double,Double,Double), RNG) = {
      val (d1, r1) = double(rng)
      val (d2, r2) = double(r1)
      val (d3, r3) = double(r2)
      ((d1, d2, d3), r3)
    }

    def ints(count: Int)(rng: RNG): (List[Int], RNG) =
      if (count > 0) {
        val (i, r) = rng.nextInt
        val (l, rr) = ints(count - 1)(r)
        (i :: l, rr)
      } else {
        (Nil, rng)
      }

    def unit[A](a: A): Rand[A] =
      rng => (a, rng)

    def map[A,B](s: Rand[A])(f: A => B): Rand[B] =
      rng => {
        val (a, rng2) = s(rng)
        (f(a), rng2)
      }

    def nonNegativeEven: Rand[Int] =
      map(nonNegativeInt)(i => i - i % 2)

    def doubleViaMap: Rand[Double] =
      map(nonNegativeInt)(i => i/(Int.MaxValue.toDouble + 1))

    def map2[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      /* my wrong answer:
      rng => {
        val (a, rnga) = ra(rng)
        val (b, rngb) = rb(rng)
        (f(a, b), rnga)
      }
      */
      rng => {
        val (a, r1) = ra(rng)
        val (b, r2) = rb(r1)
        (f(a, b), r2)
      }

    // copied from fpinscala GitHub
    def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
      fs.foldRight(unit(List[A]()))((sa, acc) => map2(sa, acc)(_ :: _))

    val int: Rand[Int] = _.nextInt

    def _ints(count: Int): Rand[List[Int]] =
      sequence(List.fill(count)(int))

    def nonNegativeLessThan(n: Int): Rand[Int] = { rng =>
      val (i, rng2) = nonNegativeInt(rng)
      val mod = i % n
      if (i + (n-1) - mod >= 0)
        (mod, rng2)
      else nonNegativeLessThan(n)(rng)
    }

    def flatMap[A,B](f: Rand[A])(g: A => Rand[B]): Rand[B] =
      rng => {
        val (a, r1) = f(rng)
        g(a)(r1)
      }

    def nonNegativeLessThanViaFlatMap(n: Int): Rand[Int] =
      flatMap(nonNegativeInt) { i =>
        val mod = i % n
        if (i + (n-1) - mod >= 0) unit(mod) else nonNegativeLessThan(n)
      }

    def mapFM[A,B](s: Rand[A])(f: A => B): Rand[B] =
      flatMap(s)(a => unit(f(a)))

    // copied from fpinscala GitHub
    // TODO understand it
    def map2FM[A,B,C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
      flatMap(ra)(a => map(rb)(b => f(a, b)))

  }

  import State._

  case class State[S, +A](run: S => (A, S)) {
    def map[B](f: A => B): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        (f(a), s1)
      })

    def mapFM[B](f: A => B): State[S, B] =
      flatMap(a => unit(f(a)))

    def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      State(s => {
        val (a, s1) = run(s)
        val (b, s2) = sb.run(s1)
        (f(a, b), s2)
      })

    def map2FM[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
      flatMap(a => sb.map(b => f(a, b)))

    def flatMap[B](f: A => State[S, B]): State[S, B] =
      State(s => {
        val (a, s1) = run(s)
        f(a).run(s1)
      })
  }


  object State {
    type Rand[A] = State[RNG, A]

    def unit[S, A](a: A): State[S, A] =
      State(s => (a, s))

    // TODO copied from github, understand it
    // This implementation uses a loop internally and is the same recursion
    // pattern as a left fold. It is quite common with left folds to build
    // up a list in reverse order, then reverse it at the end.
    // (We could also use a collection.mutable.ListBuffer internally.)
    def sequence[S, A](sas: List[State[S, A]]): State[S, List[A]] = {
      def go(s: S, actions: List[State[S,A]], acc: List[A]): (List[A],S) =
        actions match {
          case Nil => (acc.reverse,s)
          case h :: t => h.run(s) match { case (a,s2) => go(s2, t, a :: acc) }
        }
      State((s: S) => go(s,sas,List()))
    }

    // TODO copied from github, understand it
    // We can also write the loop using a left fold. This is tail recursive like the
    // previous solution, but it reverses the list _before_ folding it instead of after.
    // You might think that this is slower than the `foldRight` solution since it
    // walks over the list twice, but it's actually faster! The `foldRight` solution
    // technically has to also walk the list twice, since it has to unravel the call
    // stack, not being tail recursive. And the call stack will be as tall as the list
    // is long.
    def sequenceViaFoldLeft[S,A](l: List[State[S, A]]): State[S, List[A]] =
      l.reverse.foldLeft(unit[S, List[A]](List()))((acc, f) => f.map2(acc)( _ :: _ ))
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)

    // Ex 6.1
    assert(RNG.nonNegativeInt(rng)._1 > 0, "nonNegativeInt test case 1")
    assert(RNG.nonNegativeInt(rng)._1 == RNG.nonNegativeInt(rng)._1, "nonNegativeInt test case 2")

    // Ex 6.2
    assert(RNG.double(rng)._1 >= 0 && RNG.double(rng)._1 < 1, "double test case 1")
    assert(RNG.double(rng)._1 == RNG.double(rng)._1, "double test case 2")
    println("All tests finished.")

    // Ex 6.3
    assert(RNG.intDouble(rng)._1._1 == RNG.doubleInt(rng)._1._2, "intDouble/doubleInt test case 1")
    assert(RNG.intDouble(rng)._1._2 == RNG.doubleInt(rng)._1._1, "intDouble/doubleInt test case 2")
    assert(RNG.double(RNG.double(RNG.double(rng)._2)._2)._1 == RNG.double3(rng)._1._3, "double3 test case 1")

    // Ex 6.4
    assert(RNG.ints(10)(rng)._1.length == 10, "ints test case 1")

    // Ex 6.5
    assert(RNG.doubleViaMap(rng) == RNG.double(rng), "doubleViaMap test case 1")

    // Ex 6.6
    assert(RNG.map2(RNG.nonNegativeEven, RNG.nonNegativeEven)(_ + _)(rng)._1 % 2 == 0, "map2 test case 1")

    // Ex 6.7
    assert(RNG.sequence(List[Rand[Double]](RNG.double, RNG.double, RNG.double))(rng)._1(2) == RNG.double3(rng)._1._3, "sequence test case 1")

    // Ex 6.8
    assert(RNG.nonNegativeLessThan(10)(rng)._1 == RNG.nonNegativeLessThanViaFlatMap(10)(rng)._1, "flatMap test case 1")

    // Ex 6.9
    assert(RNG.mapFM(RNG.double)(_ * 2)(rng) == RNG.map(RNG.double)(_ * 2)(rng), "mapFM test case 1")
    assert(RNG.map2FM(RNG.double, RNG.nonNegativeEven)(_ * _)(rng) == RNG.map2(RNG.double, RNG.nonNegativeEven)(_ * _)(rng), "map2FM test case 1")

    // Ex 6.10
    // ignored
  }
}

