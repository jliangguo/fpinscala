package fpinscala.gettingstarted


// A comment!
/* Another comment */
/** A documentation comment */
object MyModule {
  def abs(n: Int): Int =
    if (n < 0) -n
    else n

  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }

  private def formatFactorial(n: Int) = {
    val msg = "The factorial of %d is %d."
    msg.format(n, factorial(n))
  }

  // first HOF
  def formatResult(name: String, n: Int, f: Int => Int) = {
    val msg = "The %s of %d is %d."
    msg.format(name, n, f(n))
  }

  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(formatResult("factorial", 7, factorial))
  }

  // self-recursion, compiler uses while-loop for optimization
  def factorial(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, acc: Int): Int =
      if (n <= 0) acc
      else go(n - 1, n * acc)

    go(n, 1)
  }

  // 0 and 1 are the first two numbers in the sequence
  def fib(n: Int): Int = {
    @annotation.tailrec
    def go(n: Int, prev: Int, cur: Int): Int =
      if (n == 0) prev
      else go(n - 1, cur, prev + cur)

    go(n, 0, 1)
  }
}

// 单态函数
object MonomorphicBinarySearch {
  def findFirst(as: Array[String], key: String): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length - 1) -1
      else if (as(n) == key) n
      else loop(n + 1)

    loop(0)
  }
}

// 多态函数
object PolymorphicFunctions {
  def findFirst[A](as: Array[A], p: A => Boolean): Int = {
    @annotation.tailrec
    def loop(n: Int): Int =
      if (n >= as.length - 1) -1
      else if (p(as(n))) n // matched
      else loop(n + 1)

    loop(0)
  }

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def loop(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else loop(n + 1)

    loop(0)
  }

  // `=>` associates to the right
  def partial1[A,B,C](a: A, f: (A,B) => C): B => C =
    (b: B) => f(a, b)

  // 柯里化
  def curry[A,B,C](f:(A,B) => C): A => (B => C) =
    a => b => f (a, b)

  // 反柯里化
  def uncurry[A,B,C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // 将两个函数组合为一个函数
  def compose[A,B,C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))
}
