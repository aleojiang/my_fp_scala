package exercise.fpinscala.datastructures

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  15:48 2018/7/30.
  */
sealed trait Tree[+A]

case class Leaf[A](value: A) extends Tree[A]

case class Branch[A](Left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

  def apply[A](): Tree[A] = ???

  def size[A](tree: Tree[A]): Int = tree match {
    case Leaf(_) => 1
    case Branch(l, r) => size(l) + size(r)
  }


}
