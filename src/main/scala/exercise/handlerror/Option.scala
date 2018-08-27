package exercise.handlerror

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


}
