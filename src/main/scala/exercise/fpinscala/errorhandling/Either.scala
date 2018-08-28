package exercise.fpinscala.errorhandling

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  09:04 2018/8/28.
  */
sealed trait Either[+E, +A] {
  def map[B](f: A => B): Either[E, B] =
    this match {
      case Right(value) => Right(f(value))
      case Left(e) => Left(e)
    }

  /**
    * When mapping over the right side,
    * we must promote the left type parameter to some supertype,
    * to satisfy the +E co-variance annotation.
    *
    * see below flatMap, orElse, map2
    */
  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(value) => f(value)
      case Left(e) => Left(e)
    }

  def orElse[EE >: E, B >: A](ob: => Either[EE, B]): Either[EE, B] =
    this match {
      case Right(value) => Right(value)
      case Left(_) => ob
    }

  def map2[EE >: E, B, C](b: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for {
      aa <- this
      bb <- b
    } yield f(aa, bb)
}

case class Left[E](value: E) extends Either[E, Nothing]

case class Right[+A](value: A) extends Either[Nothing, A]

object Either {

  def Try[E, A](a: => A): Either[Exception, A] =
    try Right(a)
    catch {
      case e: Exception => Left(e)
    }

  // impl with map2
  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).map2(traverse(t)(f))(_ :: _)
    }

  // impl with flatMap
  def traverse_1[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as match {
      case Nil => Right(Nil)
      case h :: t => f(h).flatMap(hh => traverse(t)(f).map(hh :: _))
    }

  // impl with foldRight and map2
  def traverse_2[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((e, acc) => f(e).map2(acc)(_ :: _))


}


