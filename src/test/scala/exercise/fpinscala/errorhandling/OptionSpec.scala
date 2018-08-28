package exercise.fpinscala.errorhandling

import org.scalatest.FlatSpec

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  16:38 2018/8/27.
  */
class OptionSpec extends FlatSpec {

  "traverse" should "success" in {

    val a: List[Int] = List(1, 2, 3, 4, 5)

    val res: Option[List[String]] = Option.traverse(a)(x => Some(x.toString))

    assert(res.isInstanceOf[Option[List[String]]])
  }

  "traverse2" should "success" in {

    val a: List[Int] = List(1, 2, 3, 4, 5)

    val res: Option[List[Double]] = Option.traverse_2(a)(x => Some(x.toDouble))

    assert(res.isInstanceOf[Option[List[Double]]])
  }

  "sequence" should "success" in {
    val a: List[Option[Int]] = List(Some(1), Some(2), Some(3), Some(4), Some(5))
    val res: Option[List[Int]] = Option.sequence_3(a)

    assert(a.isInstanceOf[List[Option[Int]]])
    assert(res.isInstanceOf[Option[List[Int]]])
    assert(List(1,2,3,4,5).equals(res.getOrElse(Nil)))

    val a1: List[Option[Int]] = List(Some(1), Some(2), None, Some(4), Some(5))
    val res1: Option[List[Int]] = Option.sequence_3(a1)
    assert(a.isInstanceOf[List[Option[Int]]])
    assert(res.isInstanceOf[Option[List[Int]]])
    assert(List().equals(res1.getOrElse(Nil)))

  }


}
