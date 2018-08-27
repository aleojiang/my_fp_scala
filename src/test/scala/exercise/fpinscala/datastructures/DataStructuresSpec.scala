package exercise.fpinscala.datastructures

import org.scalatest.FlatSpec

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  10:05 2018/7/25.
  */
class DataStructuresSpec extends FlatSpec {

  "foldRight" should "print right " in {
    val data = List(1, 2, 3, 4)
    val acc = 0

    val r = List.foldRight(data, acc)(_ + _)

    assert(r == 10)

  }

  "case 2" should "be ok" in {
    val r = List.foldRight(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(s"$r")
  }

  "List.foldRight2" should "be ok" in {
    val r = List.foldRight2(List(1, 2, 3), Nil: List[Int])(Cons(_, _))
    println(s"$r")
  }

  "List.foldLeft" should "be ok" in {
    val r = List.foldLeft(List(1, 2, 3), Nil: List[Int])((a, b) => Cons(b, a))
    println(s"$r")
  }

  "List.length by foldRight" should "be ok" in {
    val r = List.length(List(1, 2, 3))
    assert(r == 3)
  }

  "List.foldLeft by foldLeft" should "be ok" in {
    val r = List.foldLeft(List(1, 2, 3), 0)((x: Int, y: Int) => x + y)
    assert(r == 6)
  }

  "append via foldLeft" should "ok" in {
    val l = List(1, 2, 3)
    val r = List(4, 5, 6)
    val x = List.appendViaFoldLeft(l, r);
    assert(x.equals(List(1, 2, 3, 4, 5, 6)))
  }

  "append via foldRight" should "ok" in {
    val l = List(1, 2, 3)
    val r = List(4, 5, 6)
    val x = List.appendViaFoldRight(l, r);
    assert(x.equals(List(1, 2, 3, 4, 5, 6)))
  }

  "concat a list of list via foldRight" should "ok" in {
    val l = List(List(1), List(2), List(3))
    val x = List.concat(l);
    assert(x.equals(List(1, 2, 3)))
  }

  "transform a list by adding 1" should "ok" in {
    val l = List(1, 2, 3)
    val x = List.map(l)(_ + 1);
    assert(x.equals(List(2, 3, 4)))
  }

  "transform a list to string" should "ok" in {
    val l = List(1, 2, 3)
    val x = List.map(l)(_.toString);
    assert(x.equals(List("1", "2", "3")))
  }

  "flatMap a list another list" should "ok" in {
    val l = List(1, 2, 3)
    val x = List.flatMap(l)(a => List(a))
    assert(x.equals(List(1, 2, 3)))
  }

  "hasSub check whether a list is a sub of another list" should "ok" in {
//    assert(List.hasSubsequence(List(1,2,3,4), List(1)))
//    assert(List.hasSubsequence(List(1,2,3,4), List(1,2)))
    assert(List.hasSubsequence(List(1,2,3,4), List(3,2)))
//    assert(List.hasSubsequence(List(1,2,3,4), List(3,2,4)))
//    assert(List.hasSubsequence(List(1,2,3,4), List(3,2,4,1)))
//    assert(!List.hasSubsequence(List(1, 2, 3, 4), List(3, 2, 4, 5)))
  }

  "tree size" should "ok" in {

    assert(true)
  }


}
