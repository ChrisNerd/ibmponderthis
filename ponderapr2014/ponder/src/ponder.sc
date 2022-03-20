import scala.concurrent.duration._
import scala.actors
import scala.concurrent._
import ExecutionContext.Implicits.global
import concurrent.Future
import concurrent.Promise
import java.math.MathContext



object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

 def compact(list: List[Int]): List[Int] = {
  list match {
//  if (list.length <= 1)
   case head :: Nil => list
   case Nil => list
   case head::tail =>
   {
    // If the first 2 items are equal, combine
    // them and recurse on the remainder of the list
    if (head==tail.head)
     (2*head) :: compact(tail.tail)
    else
     head :: compact(tail)
   }
  }
 }                                                //> compact: (list: List[Int])List[Int]
 
 compact(List(4,2,2))                             //> res0: List[Int] = List(4, 4)

 def recurse(list: List[Int], N: Int) : BigDecimal =
 {
  val lc = compact(list)
  if (lc.length == N)
   lc.max
  else
   1.0/2 * recurse(lc ::: List(2), N) + 1.0/2 * recurse(lc ::: List(4), N)
/*  {
   val p2 = 1.0/2
   val p3 = future { recurse(lc ::: List(2),N) }
   val p4 = future { recurse(lc ::: List(4),N) }

   //Gah, this will hang the computer for N=5!!!  Load average goes up around 500.
  // p2 * Await.result(p3, 10 seconds) + (1-p2) * Await.result(p4,10 seconds)
  }*/
 }                                                //> recurse: (list: List[Int], N: Int)BigDecimal

 def rationalize( p: (BigDecimal,Int)): (BigDecimal, Int) ={
 if (p._1 == p._1.toInt)
  p
 else
  rationalize(2*p._1,2*p._2)
 }                                                //> rationalize: (p: (BigDecimal, Int))(BigDecimal, Int)

 val d1 = recurse(List(),1)                       //> d1  : BigDecimal = 3.0
 rationalize(d1,1)                                //> res1: (BigDecimal, Int) = (3.0,1)
 
 val d2 = recurse(List(),2)                       //> d2  : BigDecimal = 5.5000
 rationalize(d2,1)                                //> res2: (BigDecimal, Int) = (11.0000,2)
 
 val d3 = recurse(List(),3)                       //> d3  : BigDecimal = 8.43750000000
 rationalize(d3,1)                                //> res3: (BigDecimal, Int) = (135.00000000000,16)
 
 val d4 = recurse(List(),4)                       //> d4  : BigDecimal = 11.53662109375000000000000000
 rationalize(d4,1)                                //> res4: (BigDecimal, Int) = (23627.00000000000000000000000000,2048)
 
 val d5 = recurse(List(),5)                       //> d5  : BigDecimal = 13.972360298037528991699218750000000
 rationalize(d5,1)                                //> res5: (BigDecimal, Int) = (937669227.0000000000000000000000000,67108864)
 
}