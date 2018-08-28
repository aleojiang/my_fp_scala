package exercise.fpinscala.errorhandling

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  16:07 2018/7/30.
  */
sealed trait Option[+A] {
  def map[B](f: A => B): Option[B] =
    this match {
      case None => None
      case Some(v) => Some(f(v))
    }

  def getOrElse[B >: A](ob: => B): B =
    this match {
      case None => ob
      case Some(v) => v
    }

  def flatMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] =
    this match {
      case Some(v) if f(v) => Some(v)
      case _ => None
    }

  def filterViaFlatMap(f: A => Boolean): Option[A] = {
    def f1(a: A): Option[A] = if (f(a)) Some(a) else None

    flatMap(f1)
  }

}

case class Some[+A](v: A) extends Option[A]

case object None extends Option[Nothing]

object Option {
  def mean(xs: Seq[Double]): Option[Double] =
    if (xs.isEmpty) None else Some(xs.sum / xs.length)

  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))
  }

  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    def f1(aa: A, b: Option[B]): Option[C] = b.map(bb => f(aa, bb))

    a.flatMap(aa => f1(aa, b))
  }

  def map2_1[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] = {
    a.flatMap(aa => b.map(bb => f(aa, bb)))
  }

  def sequence[A](list: List[Option[A]]): Option[List[A]] = {
    list match {
      case Nil => Some(Nil)
      case h :: t => h.flatMap(hh => sequence(t).map(v => hh :: v))
    }
  }

  // using foldRight, the type annotation is required here,
  // otherwise Scala would wrongly infers the result type of the fold as `Some[Nil.type]` and reports a type error (try it!).
  // This is an unfortunate consequence of Scala using subtyping to encode algebraic data types.
  def sequence_2[A](list: List[Option[A]]): Option[List[A]] = {
    //    list.foldRight[Option[List[A]]](Some(Nil))((e, acc) => map2(e, acc)((x, y) => x :: y))
    val x: Option[List[A]] = Some(Nil)
    // need tell scala explicitly the type here
    list.foldRight(x)((e, acc) => map2(e, acc)(_ :: _))
  }

  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] =
    a.foldRight[Option[List[B]]](Some(Nil))((elem, acc) => map2(f(elem), acc)((h, tail) => h :: tail))

  def traverse_2[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = a match {
    case Nil => Some(Nil)
    case h :: t => f(h).flatMap(hh => traverse(t)(f).map(tt => hh :: tt))
  }

  def sequence_3[A](list: List[Option[A]]): Option[List[A]] =
    traverse(list)(a => a)

  /**
    * A for-comprehension consists of a sequence of bindings,
    * like aa <- a, followed by a yield after the closing brace,
    * where the yield may make use of any of the values on the left side of any previous <- binding.
    * The compiler de-sugars the bindings to flatMap calls, with the final binding and yield being converted to a call to map.
    */
  def map2_for[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for {
      aa <- a
      bb <- b
    } yield f(aa, bb)

}
