

import scala.collection.mutable.Map
import scala.collection.generic.Sorted

object ponder {
 println("Welcome to the Scala worksheet")        //> Welcome to the Scala worksheet
 
 
 val numsSumCache = collection.mutable.Map[(Int, Int, Int),List[List[Int]]]()
                                                  //> numsSumCache  : scala.collection.mutable.Map[(Int, Int, Int),List[List[Int]]
                                                  //| ] = Map()
 
 def numsThatSum(n: Int, k: Int, minel: Int) : List[List[Int]] = {
	if (k==1)
	 List(List(n))
	else
	{
	 val a = for {i <- minel to math.floor(n/k).toInt
	 row <- numsSumCache.getOrElseUpdate((n-i, k-1, i),numsThatSum(n-i, k-1, i))
	 }
	 yield if (i!=1) i :: row else row
	 numsSumCache.getOrElseUpdate((n,k,minel),a.toList)
	//a.toList
	 }
  }                                               //> numsThatSum: (n: Int, k: Int, minel: Int)List[List[Int]]

 def checkN(N: Int, k: Int, numTeams: Int, members: List[Int]): Boolean = {
  if (members.size > numTeams)
   false
  else if (k==N)
  {
   println("Success for N=",N)
   println("with members", members.sorted)
   true
  }
  else
  {
  val candidates = /* numsThatSum(N, k, 1)*/ numsSumCache.getOrElseUpdate((N, k, 1), numsThatSum(N, k, 1))
  candidates.filter(c => ((c.filter(_ != 1) diff members).size + members.size) <= numTeams)
  .sortBy(c => (c.filter(_ != 1) diff members).size).exists(candidate =>
   {
    val canToAdd = candidate.filter(_ != 1) diff members
    checkN(N,k+1,numTeams,members++canToAdd)
   }
   )
  }
 }                                                //> checkN: (N: Int, k: Int, numTeams: Int, members: List[Int])Boolean
  for ( x0 <- 2 to 3;
  x1 <- x0 to 5;
   x2 <- x1 to 9;
  x3 <- x2 to 17;
  x4 <- x3 to 33;
  x5 <- x4 to 65;
  N <- 78 to (33+65))
  {
  val l = List(2,x0,x1,x2,x3,x4,x5)//,N)//,33,63,N)//,69)
  //l.toSet.subsets foreach( sum)
  
//  l.toSet.subsets foreach (println)
 val it= l.toSet.subsets.toList
  val subsWithSumsWithk = (it, it map (_.sum), it map (x=>if (N< x.sum) 0 else x.size + N - x.sum))
 //(it map(_.sum)).sorted.distinct
 val v = (1 to N) diff subsWithSumsWithk._3
 if (v.size == 1)
    println (N,x0,x1,x2,x3,x4,x5)
  }                                               //> (78,3,5,8,16,24,54)
   //val N = 15
  //  val l = List(2,3,5,9,N)//17,33,65)//,69)
  // val it= l.toSet.subsets.toList
  
  
// val subsWithSumsWithk = (it, it map (_.sum), it map (x=>x.size + N - x.sum))
         
 //  val itsorted= it.sortBy(x=> x.size + N - x.sum)
  // itsorted map (x=> println(x, x.size, x.sum, x.size + N - x.sum))
//subsWithSumsWithk
  
 // subsWithSumsWithk foreach (x=> println(x._1, x._2, x._3))
 //(it map(_.sum)).sorted.distinct
  //  println (N,(1 to N) diff subsWithSumsWithk._3)
         
  //for (elem <- it) println(elem.sum)
  
  for { numTeams <- 8 to 8 }   // numTeams=8 in the original problem
  {
  println("numTeams",numTeams)
  for {N <- 69 to 1+math.pow(2, numTeams-1).toInt }
//  for { N <- 1 to 1+math.pow(2, numTeams-1).toInt } // All the N that we have to try.  The upper bound is 2^(numTeams-1) because
        // The largest team =N, so there are numTeams-1 that can be selected.  This gives 2^(numTeams-1) as the number of choices.
        // Choosing 0 teams could result in the k=1 case or the k=N case.  But otherwise the selection could only cover a single k case.
        // The most number of cases is therefore 1+2^(numTeams-1), and that puts an upper bound on N.
        // For the original question numTeams=8, so the upper bound on N is 1+2^7 = 129.
        // But hopefully we can find a pattern for smaller numTeams.
{
//println("N",N)
// We'd like to find the maximum N such that there exists a set of size <= numTeams of elements >=2
// that can cover all the cases from k=1 to N.
// So for each N, we'll see if it has success or not.
// N will be a success if we can find the set that covers it.
// For each k, return the List[List]] (array) of k ints that sums to N.
// For each List, add the new items to the set.  If the set size is still <= numTeams, recurse to the next k.
// otherwise, try the next List.


 //println(numsThatSum(N,30/*kLoop*/,1))
 //checkN(N,1, numTeams,List(2,3,5,9,17,33,N))  //
  
 
  }
}                                                 //> (numTeams,8)
}