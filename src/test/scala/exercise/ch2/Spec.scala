package exercise.ch2

import org.scalatest.FlatSpec

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  13:14 2018/7/24.
  */
class Spec extends FlatSpec {

  "The 3rd fibonacci number" should "be 2" in {
    assert(ExamCh2.fib(3) == 2)
  }
  "The 4th fibonacci number" should "be 3" in {
    assert(ExamCh2.fib(4) == 3)
  }

  "sorted array" should "is in order" in {
    val data = Array(1, 2, 3, 4)
    val r = ExamCh2.isSorted1(data, (a: Int, b: Int) => a > b)
    assert(r)
  }

  "unsorted array" should "not in order" in {
    val data = Array(1, 5, 3, 4)
    val r = ExamCh2.isSorted1(data, (a: Int, b: Int) => a > b)
    assert(!r)

  }

}
