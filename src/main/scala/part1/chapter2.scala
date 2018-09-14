package part1

object chapter2 {

  /**
    * Write a recursive function to get the nth Fibonacci number (http://mng.bz/C29s).
    * The first two Fibonacci numbers are 0 and 1. The nth number is always the sum of the
    * previous two—the sequence begins 0, 1, 1, 2, 3, 5. Your definition should use a
    * local tail-recursive function.
    * def fib(n: Int): Int
    */
  def fib(n: Int): Int = {
    def go(n: Int, previous: Int, current: Int): Int =
      if (n <= 0) previous
      else go(n - 1, current, previous + current)

    go(n, 0, 1)
  }

  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "%d%s %d"
    msg.format(n, name, f(n))
  }

  def main(args: Array[String]) = {
    //println(factorial(5))
    println(formatResult("nth Fibonacci number is", 8, fib))

  }

}
