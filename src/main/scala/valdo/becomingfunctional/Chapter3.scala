import scala.annotation.tailrec

object Chapter3 {

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t )=> h + List.sum(t)
    case _ => 101
  }

  def main(args: Array[String]) {
    println(List.foldRight(List(1, 2, 3), Nil:List[Int])(Cons(_, _)))
    println("Len with fold right " + List.lengthWithFoldRight(List(1,2,3,4)))
    println("Len with fold left " + List.lengthWithFoldRight(List(1,2,3,4,5)))
    println("Sum with fold left " + List.sumWithFoldLeft(List(5,5,5)))
    println("Product with fold left " + List.productWithFoldLeft(List(5,5,5)))
    println("Reverse " + List.reverse(List(1,2,3,4)))
    println("Fold right using fold left" + List.foldRightUsingFoldLeft(List(1,2,3,4), Nil:List[Int])(Cons(_, _)))
    println("Append with fold right " + List.append(List(1,2,3), List(4)))
    println("Concat " + List.concat(List(List(1),List(2), List(3, 4))))
  }
}

sealed trait List[+A]
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

case object List {
  def tail[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(_, tail) => tail
  }

  def setHead[A](xs: List[A], head: A): List[A]= xs match {
    case Nil => Nil
    case Cons(_, tail) => Cons(head, tail)
  }

  def drop[A](xs: List[A], n: Int): List[A] = if (n == 0) xs
  else xs match {
    case Nil => Nil
    case _ => drop(tail(xs), n - 1)
  }

  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] = xs match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) dropWhile(tail, f) else xs
  }

  def init[A](xs: List[A]): List[A] = xs match {
    case Nil => Nil
    case Cons(head, Nil) => Nil
    case Cons(head, tail) => Cons(head, init(tail))
  }

  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1
    case Cons(0.0, _) => 0
    case Cons(x, xs) => x * product(xs)
  }

  def foldRight[A, B](xs: List[A], z: B)(f: (A, B) => B): B = {
    xs match {
      case Nil => z
      case Cons(head, tail) => f(head, foldRight(tail, z)(f))
    }
  }

  @tailrec
  def foldLeft[A, B](xs: List[A], z: B)(f: (B, A) => B): B = xs match {
      case Nil => z
      case Cons(head, tail) => foldLeft(tail, f(z, head))(f)
    }

  def lengthWithFoldRight[A](as: List[A]): Int = foldRight(as, 0)((x, y) => 1 + y)

  def sumWithFoldLeft(as: List[Int]): Int = foldLeft(as, 0)(_ + _)
  def productWithFoldLeft(as: List[Double]): Double = foldLeft(as, 1.0)(_ * _)
  def lengthWithFoldLeft[A](as: List[A]): Int = foldLeft(as, 0)((x, y) => x + 1)

  def reverse[A](as: List[A]): List[A] = foldLeft(as, Nil:List[A])((x, y) => Cons(y, x))

  def foldRightUsingFoldLeft[A, B](as: List[A], z: B)(f: (A, B) => B) = foldLeft(reverse(as), z)((x, y) => f(y, x))

  def append[A](as: List[A], e: List[A]): List[A] = foldRight(as, e)(Cons(_, _))

  def concat[A](a: List[List[A]]): List[A] = foldRight(a, Nil:List[A])(append)

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
