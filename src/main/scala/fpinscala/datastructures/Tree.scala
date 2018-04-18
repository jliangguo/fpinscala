package fpinscala.datastructures

sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

/**
  * Created by andy on 16/6/11.
  */
object Tree {
  def size[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 1
    case Branch(l, r) => 1 + size(l) + size(r)
  }

  def maxium(t: Tree[Int]): Int = t match {
    case Leaf(n) => n
    case Branch(l, r) => maxium(l) max maxium(r)
  }

  def depth[A](t: Tree[A]): Int = t match {
    case Leaf(_) => 0
    case Branch(l, r) => 1 + (depth(l) max depth(r))
  }

  def map[A, B](t: Tree[A])(f: A => B): Tree[B] = t match {
    case Leaf(n) => Leaf(f(n))
    case Branch(l, r) => Branch(map(l)(f), map(r)(f))
  }

  def fold[A, B](t: Tree[A])(f: A => B)(g: (B, B) => B): B = t match {
    case Leaf(n) => f(n)
    case Branch(l, r) => g(fold(l)(f)(g), fold(r)(f)(g))
  }

  def sizeWithFold[A](t: Tree[A]): Int = fold(t)(a => 1)(1 + _ + _)

  def maxiumWithFold(t: Tree[Int]): Int = fold(t)(a => a)(_ max _)

  def depthWithFold[A](t: Tree[A]): Int = fold(t)(a => 0)((d1, d2) => 1 + (d1 max d2))
}
