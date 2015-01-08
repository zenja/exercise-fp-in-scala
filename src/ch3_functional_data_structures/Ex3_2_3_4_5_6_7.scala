package ch3_functional_data_structures

/**
 * Ex 3.2
 * Implement the function tail for removing the first element of a List. Note that the
 * function takes constant time. What are different choices you could make in your
 * implementation if the List is Nil? We’ll return to this question in the next chapter.
 *
 * Ex 3.3
 * Using the same idea, implement the function setHead for replacing the first element
 * of a List with a different value.
 *
 * Ex 3.4
 * Generalize tail to the function drop, which removes the first n elements from a list.
 * Note that this function takes time proportional only to the number of elements being
 * dropped—we don’t need to make a copy of the entire List.
 *
 * def drop[A](l: List[A], n: Int): List[A]
 *
 * Ex 3.5
 * Implement dropWhile, which removes elements from the List prefix as long as they
 * match a predicate.
 *
 * def dropWhile[A](l: List[A], f: A => Boolean): List[A]
 *
 * Ex 3.6
 * Not everything works out so nicely. Implement a function, init, that returns a List
 * consisting of all but the last element of a List. So, given List(1,2,3,4), init will
 * return List(1,2,3). Why can’t this function be implemented in constant time like
 * tail?
 *
 * def init[A](l: List[A]): List[A]
 *
 * Ex 3.7
 * Can product, implemented using foldRight, immediately halt the recursion and
 * return 0.0 if it encounters a 0.0? Why or why not? Consider how any short-circuiting
 * might work if you call foldRight with a large list.
 * This is a deeper question that we’ll return to in chapter 5.
 */
object Ex3_2_3_4_5_6_7 {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

//    def sum(ints: List[Int]): Int = ints match {
//      case Nil => 0
//      case Cons(x, xs) => x + sum(xs)
//    }

    def sum(ints: List[Int]): Int = List.foldRight(ints, 0)(_ + _)

//    def product(ds: List[Double]): Double = ds match {
//      case Nil => 1.0
//      case Cons(0.0, _) => 0.0
//      case Cons(x, xs) => x * product(xs)
//    }

    def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    def drop[A](l: List[A], n: Int): List[A] = l match {
      case Nil => Nil
      case Cons(_, _) =>
        if (n <= 0) l
        else drop(List.tail(l), n - 1)
    }

    def dropWhile[A](l: List[A])(f: A => Boolean): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) =>
        if (f(x)) dropWhile(xs)(f)
        else l
    }

    def setHead[A](l: List[A], head: A): List[A] = l match {
      case Nil => Cons(head, Nil)
      case Cons(_, xs) => Cons(head, xs)
    }

    def append[A](a1: List[A], a2: List[A]): List[A] =
      a1 match {
        case Nil => a2
        case Cons(h,t) => Cons(h, append(t, a2))
      }

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }
  }

  def main(args: Array[String]): Unit = {
    // Ex 3.2
    println()
    println("Ex 3.2")
    val l = List(1,2,3,4,5)
    println(List.tail(Nil))
    println(List.tail(l))

    // Ex 3.3
    println()
    println("Ex 3.3")
    println(List.setHead(Nil, 10))
    println(List.setHead(l, 999))

    // Ex 3.4
    println()
    println("Ex 3.4")
    println(List.drop(Nil, 0))
    println(List.drop(Nil, 10))
    println(List.drop(List(1,2,3,4,5), 3))

    // Ex 3.5
    println()
    println("Ex 3.5")
    println(List.dropWhile(List(1,2,3,4,5))(_ < 3))

    // Ex 3.6
    println()
    println("Ex 3.6")
    println(List.init(List(1,2,3,4,5)))

    // Ex 3.7
    println()
    println("Ex 3.7")
    // test sum and product after rewrote with foldRight
    println(s"1 + 2 + 3 = ${List.sum(List(1,2,3))}")
    println(s"1 * 2 * 3 = ${List.product(List(1,2,3))}")
    // to answer the question:
    // using foldRight(), product() cannot have short-circuit conditions
    // TODO I cannot explain the reason
  }

}
