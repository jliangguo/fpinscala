package fpinscala.datastructures

import scala.annotation.tailrec

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

/**
  * Created by andy on 16/6/11.
  */
object List {

  // use pattern matching to add up a list of integers
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  // inner product
  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(ds)
  }

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  def append[A](a1: List[A], a2: List[A]): List[A] =
    a1 match {
      case Nil => a2
      case Cons(h, t) => Cons(h, append(t, a2))
    }

  def foldRight[A, B](as: List[A], z: B)(f: (A, B) => B): B = // Utility functions
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }

  def sum2(ns: List[Int]) =
    foldRight(ns, 0)((x, y) => x + y)

  def product2(ns: List[Double]) =
    foldRight(ns, 1.0)(_ * _) // `_ * _` is more concise notation for `(x,y) => x * y`; see sidebar

  // ================== 数据共享 ==================

  def tail[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("tail of empty list")
    case Cons(_, t) => t
  }

  def setHead[A](l: List[A], h: A): List[A] = l match {
    case Nil => sys.error("setHead of empty list")
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n - 1)
    }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = l match {
    case Cons(h, t) if f(h) => dropWhile(t, f)
    case _ => l
  }

  def init[A](l: List[A]): List[A] = l match {
    case Nil => sys.error("init of empty list")
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  def length[A](l: List[A]): Int = foldRight(l, 0)((_, acc) => acc + 1)

  @tailrec
  def foldLeft[A, B](l: List[A], z: B)(f: (B, A) => B): B = l match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  def sum3(l: List[Int]) = foldLeft(l, 0)(_ + _)

  def product3(l: List[Double]) = foldLeft(l, 1.0)(_ * _)

  def reverse[A](l: List[A]): List[A] = foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  /*
  The implementation of `foldRight` in terms of `reverse` and `foldLeft` is a common trick for avoiding stack overflows
  when implementing a strict `foldRight` function as we've done in this chapter. (We'll revisit this in a later chapter,
  when we discuss laziness).

  The other implementations build up a chain of functions which, when called, results in the operations being performed
  with the correct associativity. We are calling `foldRight` with the `B` type being instantiated to `B => B`, then
  calling the built up function with the `z` argument. Try expanding the definitions by substituting equals for equals
  using a simple example, like `foldLeft(List(1,2,3), 0)(_ + _)` if this isn't clear. Note these implementations are
  more of theoretical interest - they aren't stack-safe and won't work for large lists.
  */

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft2[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)

  def foldLeftViaFoldRight[A, B](l: List[A], z: B)(f: (B, A) => B): B =
    foldRight(l, (b: B) => b)((a, g) => b => g(f(b, a)))(z)

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)(Cons(_, _))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((acc, h) => Cons(h, acc))

  def appendViaFoldLeft2[A](l: List[A], r: List[A]): List[A] =
    foldLeft(l, (b: List[A]) => b)((g, h) => b => g(Cons(h, b)))(r)

  def appendViaFoldLeft3[A](l: List[A], r: List[A]): List[A] =
    foldRightViaFoldLeft2(l, r)(Cons(_, _))

  def concat[A](l: List[List[A]]): List[A] =
    foldRight(l, Nil: List[A])(append)

  def add1(l: List[Int]): List[Int] =
    foldRight(l, Nil:List[Int])((h, t) => Cons(h+1, t))

   def doubleToString(l: List[Double]): List[String] =
     foldRight(l, Nil: List[String])((h, t) => Cons(h.toString, t))

  /*
  A natural solution is using `foldRight`, but our implementation of `foldRight` is not stack-safe. We can
  use `foldRightViaFoldLeft` to avoid the stack overflow (variation 1), but more commonly, with our current
  implementation of `List`, `map` will just be implemented using local mutation (variation 2). Again, note that the
  mutation isn't observable outside the function, since we're only mutating a buffer that we've allocated.
  */

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRight(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def map1[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, Nil:List[B])((h, t) => Cons(f(h), t))

  def map2[A, B](l: List[A])(f: A => B): List[B] = {
    val buf = new collection.mutable.ListBuffer[B]
    def go(l: List[A]):Unit = l match {
      case Nil => ()
      case Cons(h, t) =>
        buf += f(h)
        go(t)
    }
    go(l)
    List(buf.toList: _*) // converting from the standard Scala List to the list we've defined here
  }

  def filter[A](l: List[A])(f: A => Boolean): List[A] =
    foldRight(l, Nil: List[A])((h, t) => if(f(h)) Cons(h, t) else t)

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    concat(map(l)(f))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if(f(a)) List(a) else Nil)

  /*
  To match on multiple values, we can put the values into a pair and match on the pair, as shown next, and the same
  syntax extends to matching on N values (see sidebar "Pairs and tuples in Scala" for more about pair and tuple
  objects). You can also (somewhat less conveniently, but a bit more efficiently) nest pattern matches: on the
  right hand side of the `=>`, simply begin another `match` expression. The inner `match` will have access to all the
  variables introduced in the outer `match`.

  The discussion about stack usage from the explanation of `map` also applies here.
  */
  def addPairwise(a: List[Int], b: List[Int]): List[Int] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1+h2, addPairwise(t1, t2))
  }

  // addPairwise的泛化
  def zipWith[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a, b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }

  @tailrec
  def startWith[A](l: List[A], prefix: List[A]): Boolean = (l, prefix) match {
    case (_, Nil) => true
    case (Cons(h, t), Cons(h2, t2)) if h == h2 => startWith(t, t2)
    case _ => false
  }

  @tailrec
  def hasSubSequence[A](sup: List[A], sub: List[A]): Boolean = sup match {
    case Nil => sub == Nil
    case _ if startWith(sup, sub) => true
    case Cons(h, t) => hasSubSequence(t, sub)
  }
}




