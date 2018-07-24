package exercise.ch2

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  13:00 2018/7/24.
  */
object ExamCh2 {
  def fib(n: Int): Int = {
    n match {
      case 0 => 0
      case 1 => 1
      case _ => fib(n - 1) + fib(n - 2)
    }
  }

  def isSorted1[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(h: A, t: Array[A]): Boolean = {
      t match {
        case Array() => true
        case _ if gt(h, t.head) => false
        case _ => go(t.head, t.tail)
      }
    }

    go(as.head, as.tail)
  }


  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean = {
    @annotation.tailrec
    def go(n: Int): Boolean =
      if (n >= as.length - 1) true
      else if (gt(as(n), as(n + 1))) false
      else go(n + 1)

    go(0)
  }

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C = {
    b: B => f(a, b)
  }

  // since '=>' associates left to right so it is allowed to
  def curry[A, B, C](f: (A, B) => C): A => B => C = {
    a: A => b: B => f(a, b)
  }

  // as f(a) is another function f1: b=>c, so f(a) = f1 and f1(b)=>c
  def uncurry[A, B, C](f: A => B => C): (A, B) => C = {
    (a: A, b: B) => f(a)(b)
  }

  /*
 NB: There is a method on the `Function` object in the standard library,
 `Function.uncurried` that you can use for uncurrying.
 Note that we can go back and forth between the two forms. We can curry
 and uncurry and the two forms are in some sense "the same". In FP jargon,
 we say that they are _isomorphic_ ("iso" = same; "morphe" = shape, form),
 a term we inherit from category theory.
 */


  def compose[A, B, C](f: B => C, g: A => B): A => C = {
    a: A => f(g(a))
  }


}
