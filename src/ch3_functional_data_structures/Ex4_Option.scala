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
  }
}
