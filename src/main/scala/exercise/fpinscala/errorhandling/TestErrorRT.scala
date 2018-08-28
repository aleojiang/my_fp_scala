package exercise.fpinscala.errorhandling

/**
  * Created by: patrick.jiang@activenetwork.com 
  * Created on:  12:48 2018/8/2.
  */
object TestErrorRT extends App {

  def failingFn(i: Int): Int = {
    try {
      val x = 42 + 5
      x + ((throw new Exception("fail!")): Int)
    }
    catch {
      case e: Exception => 43
    }
  }

  println(failingFn(10))

}
