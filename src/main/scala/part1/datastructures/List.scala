package part1.datastructures

/**
  * Created by mccakir on 17.09.2018
  */

// List data type, parameterized on a type, A
sealed trait List[+A]

// A List data constructor representing the empty list
case object Nil extends List[Nothing]

// Another data constructor, representing nonempty list.
// Note that tail is another List[A], which may be Nil or another Cons
case class Cons[+A](head: A, tail: List[A]) extends List[A]


// List companion object. Contains functions for creating and working with lists
object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x * sum(xs)
  }


  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  // Variadic function syntax
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
}
