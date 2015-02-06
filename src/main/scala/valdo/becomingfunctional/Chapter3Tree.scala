package valdo.becomingfunctional

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {
  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + size(left) + size(right)
  }

  def max(tree: Tree[Int]): Int = tree match {
    case Leaf(value) => value
    case Branch(left, right) => max(left).max(max(right))
  }

  def depth[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(left, right) => 1 + depth(left).max(depth(right))
  }

  def map[A, B](tree: Tree[A])(f: A => B): Tree[B] = tree match {
    case Leaf(value) => Leaf(f(value))
    case Branch(left, right) => Branch(map(left)(f), map(right)(f))
  }

  def fold[A, B](tree: Tree[A])(f: A => B)(g: (B,B) => B): B = tree match {
    case Leaf(value) => f(value)
    case Branch(left, right) => g(fold(left)(f)(g), fold(right)(f)(g))
  }
}

object Chapter3Tree {
  def main(args: Array[String]) {
    val tree = Branch(Branch(Leaf(1), Leaf(2)), Branch(Branch(Leaf(3), Leaf(4)), Leaf(5)))
    println("size " + Tree.size(tree))
    println("max " + Tree.max(tree))
    println("depth " + Tree.depth(tree))
    println("map " + Tree.map(tree)(_ * 2))

    println("size fold " + Tree.fold(tree)(_ => 1)(_ + _ + 1))
    println("max fold " + Tree.fold(tree)(x => x)(_.max(_)))
    println("depth fold " + Tree.fold(tree)(x => 1)((x,y) => x.max(y) + 1))
    println("map fold " + Tree.fold(tree)(x => Leaf(x * 2): Tree[Int])(Branch(_,_)))
  }
}
