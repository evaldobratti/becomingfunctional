
object Chapter2 {

  def fib(n: Int) = {
    @annotation.tailrec
    def go(i: Int, actual: Int, next: Int): Int = {
      if (i == n)
        actual 
      else
        go(i+1, next, actual+next)
    }

    go(0, 0, 1)
  }

  def isSorted[A](array: Array[A], ordered: (A, A) => Boolean): Boolean =
    if (array.size < 2)
      true
    else
      ordered(array(0), array(1)) && isSorted(array.drop(1), ordered)

  def curry[A,B,C](f: (A, B) => C): A => (B => C) = a => b => f(a, b)

  def uncurry[A, B, C](f: A => B => C): (A, B) => C = (a, b) => f(a)(b)

  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))
}