package exercise.fpinscala.datastructures

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  14:40 2018/7/24.
  */
sealed trait List[+A]

object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def apply[A](as: A*): List[A] = if (as.isEmpty) {
    Nil
  } else {
    Cons(as.head, apply(as.tail: _*))
  }

  def sum(data: List[Int]): Int = data match {
    case Nil => 0
    case Cons(h, t) => h + sum(t)
  }

}
