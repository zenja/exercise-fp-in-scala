package ch3_functional_data_structures

/**
 * Ex 3.25
 * Write a function size that counts the number of nodes (leaves and branches) in a tree.
 *
 * Ex 3.26
 * Write a function maximum that returns the maximum element in a Tree[Int].
 * (Note: In Scala, you can use x.max(y) or x max y to compute the maximum of two integers x andy.)
 *
 * Ex 3.27
 * Write a function depth that returns the maximum path length from the root of a tree to any leaf.
 *
 * Ex 3.28
 * Write a function map, analogous to the method of the same name on List,
 * that modifies each element in a tree with a given function.
 */
object Ex3_tree {
  sealed trait Tree[+A]
  case class Leaf[A](value: A) extends Tree[A]
  case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  object Tree {
    def numNodes[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => numNodes(l) + numNodes(r)
      }

    def maxElement(t: Tree[Int]): Int =
      t match {
        case Leaf(v) => v
        case Branch(l ,r) => maxElement(l) max maxElement(r)
      }

    def depth[A](t: Tree[A]): Int =
      t match {
        case Leaf(_) => 1
        case Branch(l, r) => 1 + (depth(l) max depth(r))
      }

    def map[A, B](t: Tree[A])(f: A => B): Tree[B] =
      t match {
        case Leaf(v) => Leaf(f(v))
        case Branch(l ,r) => Branch(map(l)(f), map(r)(f))
      }
  }

  def main(args: Array[String]): Unit = {
    // Ex 3.25
    println()
    println("Ex 3.25")
    val t1 = Branch(Leaf(1), Branch(Leaf(2), Branch(Leaf(3), Leaf(4))))
    println(s"Num of nodes for ${t1}: ${Tree.numNodes(t1)}")

    // Ex 3.26
    println()
    println("Ex 3.26")
    println(s"Max element of ${t1}: ${Tree.maxElement(t1)}")

    // Ex 3.27
    println()
    println("Ex 3.27")
    println(s"Depth of ${t1}: ${Tree.depth(t1)}")

    // Ex 3.28
    println()
    println("Ex 3.28")
    println(s"Add 1 to every element of ${t1}: ${Tree.map(t1)(_ + 1)}")
  }
}
