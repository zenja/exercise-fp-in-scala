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
 */
object Ex6_State {
  trait RNG {
    def nextInt: (Int, RNG)
  }

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
  }
}

