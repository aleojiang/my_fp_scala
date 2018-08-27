package exercise.fpinscala.datastructures

/**
  * algebraic data type
  * scala language list spec -> http://mng.bz/vu45
  *
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

  def dropWhile2[A](l: List[A])(f: A => Boolean): List[A] = l match {
    case Nil => Nil
    case Cons(h, t) if f(h) => dropWhile2(t)(f)
    case _ => l
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

  //
  def foldRight[A, B](l: List[A], acc: B)(f: (A, B) => B): B = l match {
    case Nil => acc;
    case Cons(h, t) => f(h, foldRight(t, acc)(f))
  }

  def foldRight2[A, B](l: List[A], z: B)(f: (A, B) => B): B = l match {
    case Nil => z;
    case Cons(h, t) => foldRight2(t, f(h, z))(f)
  }

  def reverse[A](l: List[A]): List[A] =
    foldLeft(l, List[A]())((acc, h) => Cons(h, acc))

  def foldRightViaFoldLeft[A, B](l: List[A], z: B)(f: (A, B) => B): B =
    foldLeft(reverse(l), z)((b, a) => f(a, b))

  def foldRightViaFoldLeft_1[A, B](l: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(l, (b: B) => b)((g, a) => b => g(f(a, b)))(z)
  }


  def product(l: List[Int])(f: (Int, Int) => Int): Int = {
    foldRight(l, 0)((x: Int, y: Int) => x * y)
  }

  def length[A](as: List[A]): Int = foldRight(as, 0)((_, acc) => acc + 1)

  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = as match {
    case Nil => z
    case Cons(h, t) => foldLeft(t, f(z, h))(f)
  }

  //  def reverse[A](l: List[A]): List[A] =
  //    foldLeft(l, List[A]())((acc, e) => Cons(e, acc))

  def appendViaFoldRight[A](l: List[A], r: List[A]): List[A] =
    foldRight(l, r)((e, acc) => Cons(e, acc))

  def appendViaFoldLeft[A](l: List[A], r: List[A]): List[A] =
    foldLeft(reverse(l), r)((acc, e) => Cons(e, acc))

  def concat[A](l: List[List[A]]) =
    foldRight(l, List[A]())((left, right) => appendViaFoldRight(left, right))

  def map[A, B](l: List[A])(f: A => B): List[B] =
    foldRightViaFoldLeft(l, List[B]())((e, acc) => Cons(f(e), acc))

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    foldRightViaFoldLeft(l, List[B]())((e, acc) => append(f(e), acc))

  def filterViaFlatMap[A](l: List[A])(f: A => Boolean): List[A] =
    flatMap(l)(a => if (f(a)) List(a) else Nil)

  def zipWith[A, B, C](l: List[A], r: List[B])(f: (A, B) => C): List[C] = (l, r) match {
    case (_, Nil) => Nil
    case (Nil, _) => Nil
    case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2)(f))
  }


  @annotation.tailrec
  def startsWith[A](l: List[A], prefix: List[A]): Boolean = (l,prefix) match {
    case (_,Nil) => true
    case (Cons(h,t),Cons(h2,t2)) if h == h2 => startsWith(t, t2)
    case _ => false
  }

  @annotation.tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => sub == Nil
    case _ if startsWith(sup, sub) => true
    case Cons(_, t) => hasSubsequence(t, sub)
  }


  def contains[A](a: List[A], v: A): Boolean = a match {
    case Nil => false
    case Cons(h, _) if h == v => true
    case Cons(_, t) => contains(t, v)
  }

  def hasSubsequence2[A](sup: List[A], sub: List[A]): Boolean = sub match {
    case Nil => true
    case Cons(h, t) if contains(sup, h) => hasSubsequence(sup, t)
    case _ => false
  }


}
