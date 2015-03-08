package ch3_functional_data_structures

import sun.reflect.generics.reflectiveObjects.NotImplementedException

/**
 * Ex 4.6
 * Implement versions of map, flatMap, orElse, and map2 on Either that operate on the Right value.
 *
 * Ex 4.7
 * Implement sequence and traverse for Either. These should return the first error that’s encountered, if there is one.
 *
 * def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]]
 * def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]]
 *
 */
object Ex4_Either {
  sealed trait Either[+E, +A] {
    def map[B](f: A => B): Either[E, B] =
      this match {
        case Left(e) => Left(e)
        case Right(a) => Right(f(a))
      }

    def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(e) => Left(e)
        case Right(a) => f(a)
      }

    def orElse[EE >: E,B >: A](b: => Either[EE, B]): Either[EE, B] =
      this match {
        case Left(_) => b
        case Right(a) => Right(a)
      }

    def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
      for {
        aa <- this
        bb <- b
      } yield f(aa, bb)
  }

  object Either {
    def sequence[E, A](es: List[Either[E, A]]): Either[E, List[A]] =
      // implementation is the same with Option.sequence
      es match {
        case Nil => Right(Nil)
        case h :: t => h flatMap (hh => sequence(t) map (hh :: _))
      }

    def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
      // implementation is the same with Option.traverse
      as match {
        case Nil => Right(Nil)
        case h :: t => f(h) flatMap (fh => traverse(t)(f) map (fh :: _))
      }
  }

  case class Left[+E](value: E) extends Either[E, Nothing]
  case class Right[+A](value: A) extends Either[Nothing, A]

  def main(args: Array[String]): Unit = {
    // Ex 4.6
    println("Ex 4.6")
    assert(Right(10).map(_ * 2) == Right(20), "map")
    assert(Right(10).flatMap(x => Right(x * 2)) == Right(20), "flatMap")
    assert(Right(10).orElse(Right(20)) == Right(10), "orElse test case 1")
    assert(Left("fail").orElse(Right(20)) == Right(20), "orElse test case 2")
    assert(Right(10).map2(Right(20))(_ + _) == Right(30), "map2 test case 1")
    assert(Left("fail").map2(Right(20))((x: Int, y: Int) => x * y) == Left("fail"), "map2 test case 1")

    // Ex 4.7
    println("Ex 4.7")
    assert(Either.sequence(List(Right(1), Right(2))) == Right(List(1, 2)))
    assert(Either.sequence(List(Right(1), Left("fail"))) == Left("fail"), "sequence test case 1")
    assert(Either.sequence(List(Right(1), Left("fail1"), Left("fail2"))) == Left("fail1"), "sequence test case 2")

    println("All tests finished.")
  }
}
