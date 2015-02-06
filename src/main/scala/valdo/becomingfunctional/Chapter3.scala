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
    println("Remove odd numbers " + List.filter(List(1,2,3,4))(_ % 2 == 0))
    println("flat map " + List.filterWithFlatMap(List(1,2,3,4))(_ % 2 == 0))
    println("plus elements " + List.plusElements(List(1,2,3), List(4,5,6)))
    println("zip With" + List.zipWith(List(1,2,3), List(4,5,6))(_ + _))
    println("hasSubsequence " + List.hasSubsequence(List(1,2,3,4), List(2,3,4)))
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

  //3.16
  def plusOneEachElement(a: List[Int]): List[Int] = a match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head + 1, plusOneEachElement(tail))
  }

  //3.17
  def doublesToString(a: List[Double]): List[String] = a match {
    case Nil => Nil
    case Cons(head, tail) => Cons(head.toString(), doublesToString(tail))
  }

  //3.18
  def map[A, B](a: List[A])(f: A => B): List[B] = a match {
    case Nil => Nil
    case Cons(head, tail) => Cons(f(head), map(tail)(f))
  }

  //3.19
  def filter[A](a: List[A])(f: A => Boolean): List[A] = a match {
    case Nil => Nil
    case Cons(head, tail) => if (f(head)) Cons(head, filter(tail)(f))
      else filter(tail)(f)
  }

  //3.20
  def flatMap[A, B](a: List[A])(f: A => List[B]): List[B] = a match {
    case Nil => Nil
    case Cons(head, tail) => append(f(head), flatMap(tail)(f))
  }

  def filterWithFlatMap[A](a: List[A])(f: A => Boolean): List[A] = flatMap(a)(i => if (f(i)) List(i) else Nil)

  //3.22
  def plusElements(a: List[Int], b: List[Int]): List[Int] = a match {
    case Nil => Nil
    case Cons(aHead, aTail) => b match {
      case Nil => Nil
      case Cons(bHead, bTail) => Cons(aHead + bHead, plusElements(aTail, bTail))
    }
  }

  //3.23
  def zipWith[A](a: List[A], b: List[A])(f: (A, A) => A): List[A] = a match {
    case Nil => Nil
    case Cons(aHead, aTail) => b match {
      case Nil => Nil
      case Cons(bHead, bTail) => Cons(f(aHead, bHead), zipWith(aTail, bTail)(f))
    }
  }

  //3.24
  def hasSubsequence[A](a: List[A], sub: List[A]): Boolean = {
    a match {
      case Nil => sub match {
        case Nil => true
        case _ => false
      }
      case Cons(aHead, aTail) => sub match {
        case Nil => true
        case Cons(sHead, sTail) => if (aHead == sHead) hasSubsequence(aTail, sTail)
          else hasSubsequence(aTail, sub)
      }
    }
  }

  def apply[A](as: A*): List[A] = {
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))
  }

}
