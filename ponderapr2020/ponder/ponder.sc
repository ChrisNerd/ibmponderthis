
import sys.process._


object ponder {
  // So there are 8 choose 2 = 28 connections
  // So there are 2^28 possible graphs = 286 million
  
  // We are looking for the 8x8 binary matrix A, such that
  // A has 1s along the diagonal and 0.1 or 0 everywhere else.
  // A must at least be strongly connected (there needs to be at least 7 edges)
  // A must not be 8x8, it could be less. 4x4, 5x5,...
  // If we start with 7x7, there are only 7 choose 2 = 21 connectinos
  // 2^21 is roughly 1 million.
  // We can whip through all those.
  
  // (A^10* 1:zeros(7) ).product.... I think.  = .70
  
  
  
  /* Hmmmm, ok say we have two infected friends.
  Each provides a 10% chance of infecting us.
  This is not 20%!!!
  It is 90% * 90% we won't get infected, so
  1- .9*.9, we will.
  
  81% we won't. 19% we will.
  
  So our infected vector starts with [1,0,0,0....]
  and if our adjancy matrix is
  [0 ...
  [1...
  [1 ...
  [0....
  
  then hmmmm,
  say person 4 has the following adjacency list
  N = [0 1 1 0 ...] (we give the diagonal entries 0 because there's no self-contagion... no update based solely on whether you had it or not).
  and the probabily vector of everyone so far is
  P(time = i) = [p0i, p1i, p2i, ...]
  then the probability that person 4 will have it at time i+1 is
  1 - [p0i * (1-.1) * p1i(1-.1) *p2i ... we need to skip p4i but somehow account for it!!!!...]
  I guess we do ---
    a) we have p4i probability of already being infected
  and
    b) (1-p4i) probability that we aren't.
  If we are in a) we don't update (we multiply by 1, since there's no recovery in this model).
  if we are in b) we use 1 - [p0i * (1-.1) * p1i(1-.1) *p2i ... we need to skip p4i]
  Since a) and b) are mutually exclusive, we can add them.
  p4(i+1) = p4i * 1 + (1-p4i) * ( 1 - [p0i * (1-.1) * p1i(1-.1) *p2i ... we need to skip p4i])
  
  If we use the binary matrix we can do
  ..... N(0)*p0i*.9 * N(1)*p1i*.9... and don't have to worry about skipping p4i, since N(4 or 3 or whatever) = 0.... WHOOPS! We can't do that, we'd multiply by 0 for sure!
 
  I don't know if there's a matrix multiplication way of doing this.... might be simpler to just filter.
  val probabilityOfNotGettingIt = N zip P.filter( _._1 == 1)
    .map(_._2 * .9).prod
  p4i * 1 + (1-p4i)* (1-probabilityOfNotGettingIt)
  
  
  OK. New start. Say you have probability of being infected p1, and you enter a room with a person with a probability of being infected p2.
  There are 4 possibilities
                           them
                           non infected (1-p2)     infected (p2)
  you   non infected(1-p1) (0, 0)                  (.1, 1)
        infected (p1)      (1,.1)                  (1,  1)
  
    (you, them) probability that you and them are infected after meeting.
   We think of the four cases as if there's 100% you are both not infected, or 100% one of you is, etc.
   Then we multiply the p1s and p2s to weight each scenario.
   So, as a sanity check, if p2==0, then the right column disappears. We are left with our (p1, .1* p1), which is as expected.
   The full solution is
   (1-p1)(1-p2) * (0,0) + (1-p1)(p2)(.1,1) + (p1)(1-p2)(1,.1) + (p1)(p2)(1,1)
   
   Now we must move on to consider when you (person 1 with possibility of infection p1) meet a sequence of people (p2,p3...). What is your new p1?
   We know that in order to be not infected, we must not be infected by any of them.
   The probability of not being infected is just the complement of what we found a second ago. Namely
   (1-p1new)(based on meeting person2) = (1-p1)(1-p2) * (1,1) + (1-p1)(p2)(.9,0) + (p1)(1-p2)(0,.9) + (p1)(p2)(0,0)
   
   Now we can multiply all the meetings together (all the "didn't get infected probabilities")
   Let's factor while we go, and only focus on p1.
   (1-p1new) = (1-p1) ((The probability that we weren't infected to begin with...)) *
   ( (1-p2) (the probability that p2 isn't infected) + p2*.9 (the probability that they are but you don't get it from them)) *
   ( (1-p3) (the probability that p3 isn't infected) + p3*.9 (the probability that they are but you don't get it from them)) ....
   
   Stated another way, start with (1-p) not infected, and the only way you'll get it is if the other person is and you have a 10% shot.
   So (1- p2*.1)*(1-p3*.1)...
   
   Now we use deMorgan's law the get p1new!
   val probabilityOfNotGettingIt = (1 - p4i) * ( N zip P.filter( _._1 == true) // we filter for connections only
    .map{ case (_, pother) => 1-pother* .1}.prod)

   1-probabilityOfNotGettingIt
   
   
   
   Hmmmm, how about a totally new approach.
   
   With 8 nodes, each with 2 possibilities, we can make a 64 long binary vector to represent each state.
   (We really only need a 32 bit vector since person 1 is always infected).
   At t=0, we are in 10000000.
   Then we can make a Markov chain between the states.
   The absorbing state is 11111111.
   
   The transition matrix from state A to B  will have element 0 when any of the bits go from 1 to 0.
   Otherwise, for each bit that goes from 0 to 1, and it has n infected neighbours, transition probability is 1-.9^n
   
  What about transitions where 2 new people get infected???
  
  Say an infected person has n neighbours. That means either 0, 1, 2... n of them will be infected. So there are 2^n, or the Power Set of them.
  
  I think our algorithm is as follows:
  make a set of uninfected people U, such that they all have at least one infected neighbour.
  Call the set of infected people I.
  Iterate through U, and enter the transition probabilities as 1-.9^n, where n is each infected neighbour count.
  Since infections events are independent, we can join their probabilities by multiplying.
  
  Take the power set of U, call it P. Iterate through P, (ignoring the empty set for now, we'll get it by substraction in the end),
  mark the transition from I to (I+p) as product over the elements of p ( infection probability of each individual)
  Now we can calculate the probablity that we didn't change states as 1 - P(where p.size >=1)
  
  setofPairs looks like this:
  Set(Set(0, 2), Set(6, 2), Set(5, 6), Set(1, 3), Set(5, 4), Set(1, 6), Set(1, 2), Set(7, 3))
    
  */
  
 // BitSet().range(0, n+1)
  
  def createMarkovMatrixFromPairs( setOfPairs: Set[Set[Int]], patientZero: Int ): List[List[Double]] = {
   val n = if (setOfPairs.isEmpty) -1 else setOfPairs.map( _.max).max
  
   val adjacencyList = (0 to n).map( y=> setOfPairs.filter( _.contains(y)).flatten - y)
   //BitSet().range(0, n+1)
   //val s: BitSet = (1 to n).map(identity)(breakOut)
   val sources = ((0 to n).toSet - patientZero).subsets.toList  // This is 1 to n, excluding 0. Person 0 is patient 0, so to speak.
//   println("Sources should be 2^(n-1) " + sources.size)
   
   sources.map{ infectedSet =>
   /* val destinationStatesSet = sources.filter( dest => (infectedSet -- dest).isEmpty)
  // println("infected Set " + infectedSet)
   //println("destiState Set " + destinationStatesSet)

    val oneNeighbourInfectedOnly = destinationStatesSet.filter( dest => (dest --infectedSet).size == 1).map( dest => (dest -- infectedSet).head)
    //println("one neigh infected only " + oneNeighbourInfectedOnly)
    
    val oneNeighbourInfectedOnlyRateMap = oneNeighbourInfectedOnly
    .map( dest => (1 - math.pow(.9,adjacencyList(dest).intersect(infectedSet + patientZero ).size)))
   // println("one neigh infect only rate map " + oneNeighbourInfectedOnlyRateMap)
    
    
    val m1 = oneNeighbourInfectedOnly zip oneNeighbourInfectedOnlyRateMap toMap
    
   // println("m1 " + m1)
    
    val mMultipleStateChanges = destinationStatesSet.filter( dest => (dest --infectedSet).size >= 2)
     .map{ dest2 =>
     (dest2,
      {
       val destSet = dest2 -- infectedSet
       //println("destSet " + destSet)
       destSet.toList.map(dest3 => m1.getOrElse(dest3, 0.0)).product})}.toMap
    */
    // Above is all wrong.
    
    // The probablitiy to transition to state B is not 10%! It will be 10% * 90% * 90% (to take into account C and D not getting infected!
    val neighboursOfInfected = (infectedSet + patientZero).map( adjacencyList )
		//println("neighboursOfInfected " + neighboursOfInfected)

    val neighboursOfInfectedflattend = neighboursOfInfected.flatten - patientZero -- infectedSet
    //println("neighboursOfInfectedflattend " + neighboursOfInfectedflattend)
    
    val neighboursWithInfectedCount = neighboursOfInfectedflattend.map( x => (x, 1.0 - math.pow(.9, adjacencyList(x).intersect(infectedSet+patientZero).size))  ).toMap
    //println("neighboursWithInfectedCount " + neighboursWithInfectedCount)
    
    val transitionAmounts = neighboursOfInfectedflattend.subsets.map( sub=> (sub ++ infectedSet,
      neighboursOfInfectedflattend.toList.map( neighbour =>
        if (sub.contains(neighbour))
         neighboursWithInfectedCount(neighbour)
        else
         1- neighboursWithInfectedCount(neighbour) ).product )).toMap
         
    //println("transitionAmounts " + transitionAmounts)
    
    sources.map( d => transitionAmounts.getOrElse(d, 0.0))
    /*
    println("mMulti " + mMultipleStateChanges)
    val probStay = 1.0 - m1.values.sum - mMultipleStateChanges.values.sum
    // This is probably the error head.
    // Need to
    sources.map( d => if (d == infectedSet) probStay else if (d.isEmpty) 0.0 else mMultipleStateChanges.getOrElse(d, m1.getOrElse(d.head, 0.0) ))
    */
   }
  }
	/*	(0 until n).toList.map{i =>
		 (0 until n).toList.map{ j => if (i ==j) 1 else if (setOfPairs.contains(Set(i,j))) 0.1 else 0
 	  }
   }
  } */
  
 def updateVector1Day( M: List[List[Boolean]]) = (v: List[Double]) => // List[Double] =
  {
   for { (neighboursList, i) <- M.zipWithIndex} yield {/*
    println("row " + row)
    println("i " +i)
    println("v(i) " + v(i))
    println("(row zip v) "  +  (row zip v))
    println("(row zip v).filter( _._1 ==true) "  +  (row zip v).filter( _._1 ==true))
	  println("(row zip v).filter( _._1 ==true).map(1 - _._2 *.1) " + (row zip v).filter( _._1 ==true).map( 1 - _._2 *.1))*/
	  
	  // It could be that you have a 10% chance of getting infected no matter how many of your neighbours are infected.
	  // NO! Because (1-.9^10)^3 = 0.27630348519649 < .29 .... That's just taking into account the 3 neighbours of A because exposed 10 times.
	  
    v(i) + (1-v(i)) *
           (1-(neighboursList zip v).filter( /* are they a neighbour*/ _._1 ==true).map( /* will we not be infected by them */ 1 -  _._2 *.1).product)
   }
  }
  
  def after10Days( M: List[List[Boolean]] ) : List[Double] ={
    val v0 = 1.0 :: List.fill(M.size-1)(0.0)
    val update10 = Function.chain(List.fill(10)(updateVector1Day(M)))
    update10(v0)
  }
  
 
   
   def multiplyRowByColumn( r : List[Double], c : List[Double]): Double =
   		r.zip(c).map( x => x._1*x._2).sum
    
   def multiplyRowByMatrix ( r: List[Double], y: List[List[Double]]): List[Double] =
     r.zipWithIndex.map{ case (relement, rindex) => multiplyRowByColumn(r, y.map(yindex => yindex(rindex)))}
    
    def multiplyRowByMatrix ( y: List[List[Double]])(r: List[Double]): List[Double] =
     r.zipWithIndex.map{ case (relement, rindex) => multiplyRowByColumn(r, y.map(yindex => yindex(rindex)))}
     
    
   def matrixMultiply(x: List[List[Double]], y: List[List[Double]]): List[List[Double]] =
     for (xrow <- x)
      yield multiplyRowByMatrix(xrow, y)

	 def after19Days(M: List[List[Double]]): List[Double] = {
    val v0 = 1.0 :: List.fill(M.size-1)(0.0)
    val update19 = Function.chain(List.fill(19)(multiplyRowByMatrix(M)_ ))
    update19(v0)
  }
  
  val Aident = List(List(1.0,0.0),List(0.0,1.0))
	
	matrixMultiply(Aident, Aident)
	
	
	// We can do ^10 efficiently by doing
  /*
   A2 = A*A
   A4 = A2*A2
   A8 = A4*A4
   A10 = A8 *A2
   */
  def matrixToPower10(m: List[List[Double]]): List[List[Double]] = {
 	 val A2 = matrixMultiply(m, m)
	 val A4 = matrixMultiply(A2, A2)
	 val A8 = matrixMultiply(A4, A4)
   matrixMultiply(A8, A2)
  }

  def matrixToPower19(m: List[List[Double]]): List[List[Double]] = {
 	 val A2 = matrixMultiply(m, m)
	 val A4 = matrixMultiply(A2, A2)
	 val A8 = matrixMultiply(A4, A4)
   val A16 = matrixMultiply(A8, A8)
   matrixMultiply(matrixMultiply(A16, A2),m)
  }


  def matrixTimesOneFollowedByZeros( m: List[List[Double]]) =
   m.map(_.head)
	
	def probabilityAllInfected(m: List[List[Double]]): Double =
	 matrixTimesOneFollowedByZeros(matrixToPower10(m)).product
	
//	def addRandomEdgeToMatrix(m: List[List[Double]]): List[List[Double]] =
  val n = 8 //  val n = setOfPairs.map( _.max).max
  def createMatrixFromPairs( setOfPairs: Set[Set[Int]] ): List[List[Double]] = {
		(0 until n).toList.map{i =>
		 (0 until n).toList.map{ j => if (i ==j) 1 else if (setOfPairs.contains(Set(i,j))) 0.1 else 0
 	  }
   }
  }
	val allPairs = (0 until n).toSet.subsets(2).toSet
	allPairs.size
	
	val rand = allPairs.subsets(8).next
	
	// Looks good so far.
	val adjacencyList = (0 until n).map( y=> rand.filter( _.contains(y)).flatten - y)
                                                  
  /*val markovMat = createMarkovMatrixFromPairs(rand)
  val markovToThe10th = matrixToPower10(markovMat)
  markovToThe10th(0).last
	*/
  
  // With 8 nodes, we can represent it with 7 bit vector with starting vector:
  // 0000000
  // There will be 32 entries
  // We can do
  // val indexes = (0 to 7).toSet.subsets.toList
  // indexes.map(....)
  // Now we don't have to bother with converting to binary...
  
  
  // Going to want to create a 2^(n-1) x 2^(n-1) matrix
  // How to zero pad, to binary string of size 8.
  
  // Keep in mind this array needs to be of size n-1, not n!!!
  n.toBinaryString.toCharArray()

  val indexes = (1 to 7).toSet.subsets().toList
  indexes.size
  

/*
  for { (0 until math.pow(2.0,n-1).toInt).map{i => (0 until math.pow(2.0,n-1).toInt).map{
    j => transitionValueFromItoJ(i,j,adjacencyList)}}
                                                  
  adjacencyList.map{	listOfNeighbours => listOfNeighbours.map{ neighbour => }}
	*/
	
	
	
	
	
	val mrand = createMatrixFromPairs(rand)
	probabilityAllInfected(mrand)
	
	

/*
	01110
10001
10010
10101
01010
*/
	val example = List(
	List(1,.1,.1,.1,0),
  List(.1,1,0,0,.1),
  List(.1,0,1,.1,0),
  List(.1,0,.1,1,.1),
  List(0,.1,0,.1,1))
  
  matrixToPower10(example)
  
  probabilityAllInfected(example)
  // If "A" is infected at time 0, after ten days, there is about a 29.16521896% probability that all five will be infected.
  
	val exampleBoolean = example.map( li => li.map( num => if( num == 0.1) true else false))
	
	val markovExamplePairs = (for {
	  (l, sourceI) <- exampleBoolean.zipWithIndex
	  (bo, destI) <- l.zipWithIndex
	  if (exampleBoolean(sourceI)(destI))
	  } yield Set(sourceI, destI)).toSet
	    val m1 = Map(1 -> 0.09999999999999998, 2 -> 0.09999999999999998, 3 -> 0.09999999999999998, 4 -> 0.0)
  Set(1,2).toList.map(dest3 => m1.getOrElse(dest3, 0.0))
  Set(1,2).toList.map(dest3 => m1.getOrElse(dest3, 0.0)).product
  
	 val markovMatrixExample = createMarkovMatrixFromPairs(markovExamplePairs,0)

	markovMatrixExample
	 markovMatrixExample.head// .last  // This should be 0!!!!
	  
	 matrixToPower10(markovMatrixExample)
	 matrixToPower10(markovMatrixExample).head.last
   matrixToPower10(markovMatrixExample).last.head
	 
	  
	
	updateVector1Day(exampleBoolean)(List(1.0,0,0,0,0))
	
	after10Days(exampleBoolean)
  after10Days(exampleBoolean).product
  
  // Hmmmm, I wonder if there's some sort of conditional thing going on. Like if one person is infected it increases the likelyhood others will be.
	
	n
	val numPairs = n*(n-1)/2
	val searchSpace = math.pow(2,numPairs)
	
	/*var progress = 0
	val solution = allPairs.subsets.grouped(100000).map(_.par.find{ s=>
	if (s.size < n) false else
	{
	val probAllInfected = matrixToPower10(createMarkovMatrixFromPairs(s)).head.last
	
	 if (progress % 100000 == 0)
	 {println(progress)
	  println(s)
	  println(probAllInfected)
	  }
  progress = progress+1

  (probAllInfected - 0.7).abs < 0.00005}   }).flatten*/
  
  
                       // Jesus! There are only 2^(n-2) spanning trees!!!
 // That's only 64!!!
 // Probably even less if you consider symmetry.
 // Can we generate them all using recursion?
 // Select one node, n0.
 // List all spanning trees of S - n0.
 // List all ways of connecting n0 to each tree.

 def allSpanningTrees(S: Set[Int]): Set[Set[Set[Int]]] = S.size match {
  case 2 => Set(Set(S))
  case _ => {
   val arbitraryNode = S.head
   for {
    spanningTree <- allSpanningTrees(S-arbitraryNode)
    connectArbitraryNode <- (S-arbitraryNode).subsets.drop(1)
   } yield spanningTree ++ connectArbitraryNode.map( Set(_, arbitraryNode))
  }
 }
 2^(n-3)*((n-1)*(n-2)/2 -1)
 
 allSpanningTrees((0 to 2).toSet)
 allSpanningTrees((0 to 2).toSet).size
 
 allSpanningTrees((0 to 3).toSet)
 allSpanningTrees((0 to 3).toSet).size
 allSpanningTrees((0 to 4).toSet)
 allSpanningTrees((0 to 4).toSet).size
                       
 def addEdge(current: Set[Set[Int]]) =
  current + scala.util.Random.shuffle((0 until n).toVector).take(2).toSet

 def removeEdge(current: Set[Set[Int]]) =
  current - current.toVector(scala.util.Random.nextInt(current.size))


 val cacheOfResults: scala.collection.mutable.Map[Set[Set[Int]], Double] = scala.collection.mutable.Map()
  val allSimple8Graphs =  "geng -c 8" #| "showg -e -l0" lineStream // | showg -e -l0".!! "uname -a".!!

 val firstSet = allSimple8Graphs.grouped(4).map{ groupOf4Strings =>
 val arrayOfStrings: Array[String] = groupOf4Strings(3).split("  ")
 val arrayOfArrayOfStrings : Array[Array[String]] = arrayOfStrings.map(_.split(" "))
 val arrayOfPairs : Array[Set[Int]] = arrayOfArrayOfStrings.map{arrayOfS => arrayOfS.map(_.toInt).toSet}
 arrayOfPairs.toSet
 }


 val allGraphs = (for {
 current <- firstSet
 patientZero <- 0 to 7
} yield (current, patientZero)).toVector //, matrixToPower10(createMarkovMatrixFromPairs(current,patientZero)).head.last)
 val allGraphsWithSolution = allGraphs.par.map{ case (current, patientZero) =>
 ((current,patientZero), after19Days( createMarkovMatrixFromPairs(current, patientZero)).last) }
  //  matrixToPower19(createMarkovMatrixFromPairs(current,patientZero)).head.last) }
 
 println("allSolutions")
 allGraphsWithSolution.toVector.foreach(println)
 println("sortedSolutions")
 allGraphsWithSolution.toVector.sortBy( x => (x._2 -.7).abs ).foreach(println)
 println("done")

 var p = 0

def binarySearchOnMostlySorted(L: Int, R: Int): Option[(Set[Set[Int]], Int)] = {
 if (R < L)
  None
 else
 {
  val m = L + (R-L)/2
  val (current,patientZero) = allGraphs(m)
  val res = matrixToPower19(createMarkovMatrixFromPairs(current,patientZero)).head.last
  if (p %1==0)
  {
   println("progress " + p)
   println("current " +  current)
   println("patientZero " +  patientZero)
   println("res " + res)
  }
  p=p+1

  res match {
   case res if (res < .7 - 0.00005) =>
   {
    binarySearchOnMostlySorted(m+1, R) match
    {
     case Some(x) => Some(x)
     case None => binarySearchOnMostlySorted(L, m-1)
    }
   }
   case res if (res >= .7 + 0.00005) =>
   {
    binarySearchOnMostlySorted(L, m-1) match
    {
     case Some(x) => Some(x)
     case None => binarySearchOnMostlySorted(m+1, R)
    }
   }
   case _ => Some((current,patientZero))
  }
 }
}

val solution = binarySearchOnMostlySorted(0, allGraphs.size -1)
println(solution)
 /*
 var progress = 0
 def attempt(current: Set[Set[Int]]) : Set[Set[Int]] = {
  val res = cacheOfResults.getOrElseUpdate(current, matrixToPower10(createMarkovMatrixFromPairs(current)).head.last)
  if (progress % 100000 == 0)
	{
	 println("progress " + progress)
	 println("current " +  current)
   println("res " + res)
  }
  progress = progress+1
  res match {
   case res if (res < .7 - 0.00005) => attempt(addEdge(current))
   case res if (res > .7 + 0.00005) => attempt(removeEdge(current))
   case _ => current
  }
 }
  val solution = attempt(Set(Set(0,1)))
  solution
  */
                                                  
 //solution
	/*if (s.size < n) false else (probabilityAllInfected(createMatrixFromPairs(s)) - .7).abs*/
// None found with n= 7 and abs < 0.0001)
	// 70.00% (accurate to the second decimal digit after the decimal point)
	// 70.005
	//-70.00
	//.00 005
	   
}