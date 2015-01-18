package ch3_functional_data_structures

/**
 * Ex 4.1
 * Implement all of the preceding functions on Option. As you implement each function,
 * try to think about what it means and in what situations you’d use it.
 * We’ll explore when to use each of these functions next. Here are a few hints for solving this exercise:
 *
 * 1. It’s fine to use pattern matching,
 *    though you should be able to implement all the functions besides map
 *    and getOrElse without resorting to pattern matching.
 * 2. For map and flatMap, the type signature should be enough to determine the implementation.
 * 3. getOrElse returns the result inside the Some case of the Option, or if the Option
 *    is None, returns the given default value.
 * 4. orElse returns the first Option if it’s defined; otherwise,
 *    it returns the second Option.
 *
 * Ex 4.2
 * Implement the variance function in terms of flatMap.
 * If the mean of a sequence is m, the variance is the mean of math.pow(x - m, 2)
 * for each element x in the sequence. See the definition of variance on Wikipedia (http://mng.bz/0Qsr).
 *
 * def variance(xs: Seq[Double]): Option[Double]
 *
 * Ex 4.3
 * Write a generic function map2 that combines two Option values using a binary function.
 * If either Option value is None, then the return value is too. Here is its signature:
 *
 * def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]
 */
object Ex4_Option {
  sealed trait Option[+A] {
    def map[B](f: A => B): Option[B] = {
      this match {
        case None => None
        case Some(v) => Some(f(v))
      }
    }

    // implement flatMap by pattern matching
    def flatMap_PM[B](f: A => Option[B]): Option[B] = {
      this match {
        case None => None
        case Some(v) => f(v)
      }
    }

    def flatMap[B](f: A => Option[B]): Option[B] = {
      map(f) getOrElse None
    }

    def getOrElse[B >: A](default: => B): B = {
      this match {
        case None => default
        case Some(v) => v
      }
    }

    // implement orElse by pattern matching
    def orElse_PM[B >: A](ob: => Option[B]): Option[B] = {
      this match {
        case None => ob
        case _ => this
      }
    }

    def orElse[B >: A](ob: => Option[B]): Option[B] = {
      this map (Some(_)) getOrElse ob
    }

    def filter(f: A => Boolean): Option[A] = {
      this match {
        case None => None
        case Some(v) =>
          if (f(v)) Some(v)
          else None
      }
    }
  }

  case class Some[+A](get: A) extends Option[A]
  case object None extends Option[Nothing]

  object Option {
    def mean(xs: Seq[Double]): Option[Double] = {
      if (xs.isEmpty) None
      else Some(xs.sum / xs.length)
    }

    def variance(xs: Seq[Double]): Option[Double] = {
      mean(xs) flatMap (m => mean(xs.map(x => math.pow(x - m, 2))))
    }

    def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
      (a, b) match {
        case (Some(x), Some(y)) => Some(f(x, y))
        case _ => None
      }
      // in https://github.com/fpinscala/fpinscala/blob/master/answerkey%2Ferrorhandling%2F03.answer.scala:
      // a flatMap (aa => b map (bb => f(aa, bb)))
    }
  }

  def main(args: Array[String]): Unit = {
    val noneInt: Option[Int] = None
    // Ex 4.1
    println("Ex 4.1")
    // not None
    println("- not None")
    println(Some(10).map(_ * 2))
    println(Some(10).flatMap[Int](x => Some[Int](x * 2)))
    println(Some(10).getOrElse(100))
    println(Some(10).orElse(Some(100)))
    println(Some(10).filter(_ % 2 == 0))
    // None
    print("- None")
    println(noneInt.map(_ * 2))
    println(noneInt.flatMap[Int](x => Some[Int](x * 2)))
    println(noneInt.getOrElse(100))
    println(noneInt.orElse(Some(100)))
    println(noneInt.filter(_ % 2 == 0))

    // Ex 4.2
    println("\nEx4.2")
    println(s"Var(1,2,3,4,5): ${Option.variance(Seq(1.0, 2.0, 3.0, 4.0, 5.0))}")
    println(s"Var(empty list): ${Option.variance(Seq())}")

    // Ex 4.3
    println("\nEx4.3")
    println(Option.map2(Some(1), Some(2))(_ + _))
    println(Option.map2(Some(1), noneInt)(_ + _))
  }
}
