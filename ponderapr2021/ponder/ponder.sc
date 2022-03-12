object ponder {
 // https://en.wikipedia.org/wiki/Josephus_problem
 

 def remaining( pieces: Seq[Int], k: Int, q: Int, current_Position: Int  = 0, current_Step_Number: Int  = 0): (Seq[Int],Int) =
  if (current_Step_Number == k)
   (pieces, current_Position)
  else
  {
   // We start by advancing the position, wrapping around pieces.size if necessary.
   val advancedPosition = (current_Position - 1 + q ) % pieces.size
   // maintain the first advancedPosition indices.
   val advancedPieceRemoved = pieces.patch(advancedPosition, Nil, 1)
   // Now advancedPieceRemoved should be of 1 shorter length.
   // We should wrap advancedPosition around advancedPieceRemoved.size to get it at the 0th index instead of the nth.
   remaining(advancedPieceRemoved, k, q,
     advancedPosition % advancedPieceRemoved.size, current_Step_Number + 1)
   }
     //this is a bug for sure!!!((current_Position - 1 + q) % pieces.size)%(pieces.size - 1), current_Step_Number + 1)

 val p = 1 to 8
  
 remaining(p, 1, 5)
 remaining(p, 2, 5)
 remaining(p, 3, 5)
 remaining(p, 4, 5)
 remaining(p, 5, 5)
 remaining(p, 6, 5)
 remaining(p, 7, 5)
 // java.lang.ArithmeticException: / by zero
 // remaining(p, 8, 5)
 
 
 // Let f(N)=LCM{1,2,3..,N}
 // f(N) = product from k=1 to n of pk^ floor( ln N / ln pk )
 // where pk is the kth prime, and n is the largest natural number such than pn <= N.
 lazy val primes: Stream[Int] = 2 #:: Stream.from(3).filter(i =>
  primes.takeWhile{j => j * j <= i}.forall{ k => i % k > 0});
 def f(N: Int): Int = {
  primes.takeWhile(_ <= N).map( pk =>
   scala.math.pow(pk.toDouble, scala.math.floor( scala.math.log(N) / scala.math.log(pk))))
  .product.toInt
 }
 
 f(2)
 f(3)
 f(4)
 f(5)
 f(6)
 
 var s = Set(1, 2, 3)
 s -= 2
 val c = (1 to 3).toSeq.combinations(2)
 
  c.foreach(println)
 val all_Possible_Prize_Sets2 = (1 to 9).toSeq.combinations(2).to[collection.mutable.Set]
 val toRemove = remaining(1 to 9, 7, 5)._1.toIndexedSeq
 all_Possible_Prize_Sets2.contains(  toRemove )
 
 all_Possible_Prize_Sets2 -= toRemove
 
 


 val sol2 = for{
  n <- Stream.from(11)
  k = n-10 // 1 to n
  all_Possible_Prize_Sets = (1 to n).toSeq.combinations(k).to[collection.mutable.Set]// (1 to n).toSet.subsets(k).toSet
  devSlashNull = println("Here at n = " + n + " with allPossible Prizes Set size = " + all_Possible_Prize_Sets.size + " f(n) = " + f(n) + " head " + all_Possible_Prize_Sets.head)
  
  x = (0 until f(n)).takeWhile{ q =>
  /* val toRemove = remaining(1 to n, n - k, q)._1.toIndexedSeq
   println("toremove " + toRemove)
   println("does it contain " + all_Possible_Prize_Sets.contains(toRemove))*/
   all_Possible_Prize_Sets -= remaining(1 to n, n - k, q)._1.toIndexedSeq
   !all_Possible_Prize_Sets.isEmpty
  }
  devSlashNull2 = println("Here at n = " + n + " with allPossible Prizes Set size = " + all_Possible_Prize_Sets.size)
  
  if (!all_Possible_Prize_Sets.isEmpty)// && unWinnable_Prizes.head.size == n - 7)
 } yield (n, all_Possible_Prize_Sets)
 println(sol2.head._2.size)
 println(sol2.head._2)
 
 
 /*
 Here at n = 20 with allPossible Prizes Set size = 2
  sol2  : scala.collection.immutable.Stream[(Int, scala.collection.mutable.Set[scala.collection.immutable.IndexedSeq[Int]])] =
   Stream((20,Set(Vector(1, 2, 3, 4, 5, 6, 7, 8, 11, 14, 15, 16, 17),
   Vector(4, 5, 6, 7, 10, 13, 14, 15, 16, 17, 18, 19, 20))), ?)
   
 // For a given n, we call a set of k numbers unwinnable if no matter what q the player chooses,
 // he will not be able to win exactly this set after n-k spins.
 
 val solutions = for{
  n <- Stream.from(9)
  k <- (n-7) to (n-7) //1 to n
  all_Possible_Prize_Sets = (1 to n).toSet.subsets(k).toSet
  
  
  
  winnable_Prize_Sets = (for {
   q <- 0 until f(n) // (1 to n).product /*123*n*n*/ // n*n //
   basePrize = remaining(1 to n, n - k, q)._1
   shift_Amount <- 0 to 0
  } yield basePrize.map(x => (n + x + shift_Amount - 1)% n + 1).toSet
  ).toSet
  
  devSlashNull = println("Here at n = " + n + " with allPossible Prizes Set size = " + all_Possible_Prize_Sets.size + " winning prize set size = " + winnable_Prize_Sets.size + " f(n) = " + f(n))
  unWinnable_Prizes = all_Possible_Prize_Sets -- winnable_Prize_Sets
  if (!unWinnable_Prizes.isEmpty)// && unWinnable_Prizes.head.size == n - 7)
 } yield (n, unWinnable_Prizes)
    */


 def main(args: Array[String]): Unit = {
 
 import scala.math.Ordering.Implicits._
 sol2.take(1).foreach(s =>{
  println("For n = " + s._1 + " we have ")
  s._2.toList.map(_.toList.sorted).sorted
  .foreach( s2 => println(s2.toList.sorted))
  }
 )
 // Even with the factorial, we have the correct solution that matches the website:
 //  For n = 9 we have
 /*
 List(1, 2, 5, 8, 9)
 List(2, 3, 4, 5, 8)
 List(2, 5, 6, 7, 8)
 */
 
 
 // With q <- 0 to 11*n*n we have
// For n = 11 we have
/* List(1, 4, 8, 11)
 List(2, 8, 9, 11)
 List(4, 6, 8, 9) */
 }
}