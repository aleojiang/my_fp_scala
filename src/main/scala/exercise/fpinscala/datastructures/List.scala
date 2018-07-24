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

  def tail[A](a: List[A]): List[A] = a match {
    case Nil => Nil
    case Cons(_, t) => t
  }

  def setHead[A](a: List[A], h: A): List[A] = a match {
    case Nil => Nil
    case Cons(_, t) => Cons(h, t)
  }

  def drop[A](l: List[A], n: Int): List[A] = {
    @annotation.tailrec
    def go(a1: List[A], cnt: Int): List[A] = a1 match {
      case Nil => Nil
      case Cons(_, _) if cnt > n => a1
      case Cons(_, t) => go(t, n + 1)
    }

    go(l, 0)
  }

  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    @annotation.tailrec
    def go(a1: List[A]): List[A] = a1 match {
      case Nil => Nil
      case Cons(h, _) if f(h) => a1
      case Cons(_, t) => go(t)
    }

    go(l)
  }

  def append[A](a1: List[A], a2: List[A]): List[A] = a1 match {
    case Nil => a2
    case Cons(h, t) => Cons(h, append(t, a2))
  }

  /**
    * get the list without the last element
    *
    * @param l
    * @tparam A
    * @return
    */
  def init[A](l: List[A]): List[A] = l match {
    case Nil => Nil
    case Cons(_, Nil) => Nil
    case Cons(h, t) => Cons(h, init(t))
  }

  /**
    * by using list buffer to store temporary data
    *
    * @param l
    * @tparam A
    * @return
    */
  def init1[A](l: List[A]): List[A] = {
    import collection.mutable.ListBuffer
    val buf = new ListBuffer[A]

    @annotation.tailrec
    def go(cur: List[A]): List[A] = cur match {
      case Nil => Nil
      case Cons(_, Nil) => List(buf.toList: _*)
      case Cons(h, t) => buf += h; go(t)
    }

    go(l)
  }


}
