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
 *
 * Ex 3.8
 * See what happens when you pass Nil and Cons themselves to foldRight,
 * like this: foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)).
 * What do you think this says about the relationship between foldRight
 * and the data constructors of List?
 *
 * Ex 3.9
 * Compute the length of a list using foldRight.
 *
 * def length[A](as: List[A]): Int
 *
 * Ex 3.10
 * Our implementation of foldRight is not tail-recursive and will result in
 * a StackOverflowError for large lists (we say it’s not stack-safe).
 * Convince yourself that this is the case,
 * and then write another general list-recursion function, foldLeft,
 * that is tail-recursive, using the techniques we discussed in the previous chapter.
 * Here is its signature:
 *
 * def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B
 *
 * Ex 3.11
 * Write sum, product, and a function to compute the length of a list using foldLeft.
 *
 * Ex 3.12
 * Write a function that returns the reverse of a list (given List(1,2,3) it returns List(3,2,1)).
 * See if you can write it using a fold.
 *
 * Ex 3.13 TODO check correctness
 * Hard: Can you write foldLeft in terms of foldRight?
 * How about the other way around?
 * Implementing foldRight via foldLeft is useful because it lets us
 * implement foldRight tail-recursively, which means it works
 * even for large lists without overflowing the stack.
 *
 * Ex 3.14
 * Implement append in terms of either foldLeft or foldRight.
 *
 * Ex 3.15
 * Hard: Write a function that concatenates a list of lists into a single list.
 * Its runtime should be linear in the total length of all lists.
 * Try to use functions we have already defined.
 *
 * Ex 3.16
 * Write a function that transforms a list of integers by adding 1 to each element.
 * (Reminder: this should be a pure function that returns a new List!)
 *
 * Ex 3.17
 * Write a function that turns each value in a List[Double] into a String.
 * You can use the expression d.toString to convert some d: Double to a String.
 *
 * Ex 3.18
 * Write a function map that generalizes modifying each element in a list
 * while maintaining the structure of the list.
 * Here is its signature:
 *
 * def map[A,B](as: List[A])(f: A => B): List[B]
 *
 * Ex 3.19
 * Write a function filter that removes elements from a list
 * unless they satisfy a given predicate. Use it to remove all odd numbers from a List[Int].
 *
 * def filter[A](as: List[A])(f: A => Boolean): List[A]

 */
object Ex3_2_to_19 {
  sealed trait List[+A]

  case object Nil extends List[Nothing]

  case class Cons[+A](head: A, tail: List[A]) extends List[A]

  object List {
    def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => f(x, foldRight(xs, z)(f))
      }

    def foldLeft[A,B](as: List[A], z: B)(f: (B, A) => B): B =
      as match {
        case Nil => z
        case Cons(x, xs) => foldLeft(xs, f(z, x))(f)
      }

    // implement foldRight using foldLeft
    def foldRightL[A,B](as: List[A], z: B)(f: (A, B) => B): B =
      foldLeft(List.reverseL(as), z)((a, b) => f(b, a))

    // implement foldLeft using foldRight
    def foldLeftR[A,B](as: List[A], z: B)(f: (B, A) => B): B =
      foldRight(List.reverseL(as), z)((a, b) => f(b, a))

    def sum(ints: List[Int]): Int = List.foldRight(ints, 0)(_ + _)

    // sum() using foldLeft()
    def sum_left(ints: List[Int]): Int = List.foldLeft(ints, 0)(_ + _)

    def product(ds: List[Double]): Double = foldRight(ds, 1.0)(_ * _)

    // product() using foldLeft()
    def product_left(ds: List[Double]): Double = foldLeft(ds, 1.0)(_ * _)

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

    // implement append() with foldRight()
    def appendR[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2)((x:A, xs: List[A]) => Cons(x, xs))

    def init[A](l: List[A]): List[A] =
      l match {
        case Nil => Nil
        case Cons(x, Nil) => Nil
        case Cons(h, t) => Cons(h, init(t))
      }

    def length[A](as: List[A]): Int = foldRight(as, 0)((_, z) => z + 1)

    // TODO copied from others solution, understand it
    // reverse a list using foldRight
    def reverseR[A](as: List[A]): List[A] = foldRight[A, List[A]](as, Nil)((x, xs) => List.append(xs, List(x)))
    // reverse a list using foldLeft
    def reverseL[A](as: List[A]): List[A] = foldLeft[A, List[A]](as, Nil)((xs, x) => List.append(List(x), xs))

    // concatenates a list of lists into a single list (ex 3.15)
    def unzip[A](ll: List[List[A]]): List[A] =
      foldRight[List[A], List[A]](ll, Nil)(List.append(_, _))

    def map[A, B](l: List[A])(f: A => B): List[B] =
      l match {
        case Nil => Nil
        case Cons(x, xs) => Cons(f(x), map(xs)(f))
      }

    def filter[A](as: List[A])(f: A => Boolean): List[A] =
      as match {
        case Nil => Nil
        case Cons(x, xs) =>
          if (f(x)) Cons(x, filter(xs)(f))
          else filter(xs)(f)
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

    // Ex 3.8
    println()
    println("Ex 3.8")
    // WOW, the result of below code is Cons(1,Cons(2,Cons(3,Nil)))
    // which indicates foldRight has strong connection to List structure (head, tail)
    println(List.foldRight(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    // Ex 3.9
    println()
    println("Ex 3.9")
    println(s"Length of List(1,2,3) is ${List.length(List(1,2,3))}")

    // Ex 3.10
    println()
    println("Ex 3.10")
    def _mkstring_ints(l: List[Int]): String = {
      List.foldLeft(l, "")(_ + _)
    }
    println(s"Contents of List(1,2,3): ${_mkstring_ints(List(1,2,3))}")
    println(s"Contents of List(1): ${_mkstring_ints(List(1))}")

    // Ex 3.11
    println()
    println("Ex 3.11")
    println(s"1 + 2 + 3 = ${List.sum_left(List(1,2,3))}")
    println(s"1 * 2 * 3 = ${List.product_left(List(1,2,3))}")

    // Ex 3.12
    println()
    println("Ex 3.12")
    println(s"Reverse of List(1,2,3) is: ${List.reverseL(List(1,2,3))}")
    println(s"Reverse of List(1,2,3) is: ${List.reverseR(List(1,2,3))}")

    // Ex 3.13
    println()
    println("Ex 3.13")
    println(List.foldRightL(List(1,2,3), Nil:List[Int])(Cons(_,_)))

    // Ex 3.14
    println()
    println("Ex 3.14")
    println(s"Append List(1,2,3) and List(4,5,6): ${List.appendR(List(1,2,3), List(4,5,6))}")

    // Ex 3.15
    println()
    println("Ex 3.15")
    println(s"Unzip List(List(1,2), List(3,4), List(5,6)): ${List.unzip(List(List(1,2), List(3,4), List(5,6)))}")

    // Ex 3.16
    println()
    println("Ex 3.16")
    println(s"Transform List(1,2,3) by addind one: ${List.map(List(1,2,3))(_ + 1)}")

    // Ex 3.17 & 3.18
    println()
    println("Ex 3.17 & 3.18")
    println(s"Transform List[Double] to List[String]: ${List.map(List(1.1, 2.2, 3.3))(_.toString)}")

    // Ex 3.19
    println()
    println("Ex 3.19")
    println(s"Remove all odd numbers from List(1,2,3,4,5,6): ${List.filter(List(1,2,3,4,5,6))(_ % 2 == 0)}")
  }

}
