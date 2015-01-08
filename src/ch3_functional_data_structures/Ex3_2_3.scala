package ch3_functional_data_structures

/**
 * Ex 3.2:
 * Implement the function tail for removing the first element of a List. Note that the
 * function takes constant time. What are different choices you could make in your
 * implementation if the List is Nil? We’ll return to this question in the next chapter.
 *
 * Ex 3.3
 * Using the same idea, implement the function setHead for replacing the first element
 * of a List with a different value.
 */
object Ex3_2_3 {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def sum(ints: List[Int]): Int = ints match {
      case Nil => 0
      case Cons(x, xs) => x + sum(xs)
    }

    def product(ds: List[Double]): Double = ds match {
      case Nil => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs) => x * product(xs)
    }

    def apply[A](as: A*): List[A] =
      if (as.isEmpty) Nil
      else Cons(as.head, apply(as.tail: _*))

    def tail[A](l: List[A]): List[A] = l match {
      case Nil => Nil
      case Cons(x, xs) => xs
    }

    def setHead[A](l: List[A], head: A): List[A] = l match {
      case Nil => Cons(head, Nil)
      case Cons(_, xs) => Cons(head, xs)
    }

  }

  def main(args: Array[String]): Unit = {
    // Ex 3.2
    println("Ex 3.2")
    val l = List(1,2,3,4,5)
    println(List.tail(Nil))
    println(List.tail(l))

    // Ex 3.3
    println("Ex 3.3")
    println(List.setHead(Nil, 10))
    println(List.setHead(l, 999))
  }

}
