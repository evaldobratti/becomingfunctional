
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
}