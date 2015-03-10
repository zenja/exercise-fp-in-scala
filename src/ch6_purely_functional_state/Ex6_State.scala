package ch6_purely_functional_state

/**
 * Ex 6.1
 * Write a function that uses RNG.nextInt to generate a random integer between 0 and Int.maxValue (inclusive).
 * Make sure to handle the corner case when nextInt returns Int.MinValue,
 * which doesn’t have a non-negative counterpart.
 *
 * def nonNegativeInt(rng: RNG): (Int, RNG)
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
  }

  def main(args: Array[String]): Unit = {
    val rng = SimpleRNG(1)

    // Ex 6.1
    assert(RNG.nonNegativeInt(rng)._1 > 0, "nonNegativeInt test case 1")
    assert(RNG.nonNegativeInt(rng)._1 == RNG.nonNegativeInt(rng)._1, "nonNegativeInt test case 1")

    println("All tests finished.")
  }
}

