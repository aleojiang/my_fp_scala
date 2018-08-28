package exercise.fpinscala.errorhandling

import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  11:13 2018/8/28.
  */
class EitherSpec extends FlatSpec with Matchers {

  it should "traverse returns right list" in {
    val x1 = Either.traverse(List(1, 2, 3))(x => Right(x))

    val expected = Right(List(1, 2, 3))
    assert(x1.isInstanceOf[Right[List[Int]]])
    assertResult(expected)(x1)
  }

  it should "traverse returns NumberFormatException" in {
    val x1 = Either.traverse(List("1", "2", "abc"))(x => Either.Try(x.toInt))
    assert(x1.isInstanceOf[Left[Exception]])
    assert(x1.asInstanceOf[Left[Exception]].value.isInstanceOf[NumberFormatException])
  }

  it should "traverse_1 returns right list" in {
    val x1 = Either.traverse_1(List(1, 2, 3))(x => Right(x))

    val expected = Right(List(1, 2, 3))
    assert(x1.isInstanceOf[Right[List[Int]]])
    assertResult(expected)(x1)
  }

  it should "traverse_1 returns NumberFormatException" in {
    val x1 = Either.traverse_1(List("1", "2", "abc"))(x => Either.Try(x.toInt))
    assert(x1.isInstanceOf[Left[Exception]])
    assert(x1.asInstanceOf[Left[Exception]].value.isInstanceOf[NumberFormatException])
  }

  it should "traverse_2 returns right list" in {
    val x1 = Either.traverse_2(List(1, 2, 3))(x => Right(x))

    val expected = Right(List(1, 2, 3))
    assert(x1.isInstanceOf[Right[List[Int]]])
    assertResult(expected)(x1)
  }

  it should "traverse_2 returns NumberFormatException" in {
    val x1 = Either.traverse_2(List("1", "2", "abc"))(x => Either.Try(x.toInt))
    assert(x1.isInstanceOf[Left[Exception]])
    assert(x1.asInstanceOf[Left[Exception]].value.isInstanceOf[NumberFormatException])
  }

}
