object ponder {
  
  val A: List[Long] = List(855661, 1395050, 1402703, 1575981, 2956165, 4346904, 5516627, 5693538, 6096226, 7359806).map(_.toLong).sorted// List(1,3)
                                                  //> A  : List[Long] = List(855661, 1395050, 1402703, 1575981, 2956165, 4346904, 
                                                  //| 5516627, 5693538, 6096226, 7359806)
  
  /* Let's  work backwards
  Say for just repeating a pair a,b and a <= b, both !=0.
  That maps to 2a, b-a.
  2a != 0, but b could =a. So we want a=b.
  that implies there must be something that maps to (2a,b-a) where they equal.
  2a=b-a. => 3a=b.
  That means b = 0 (mod 3).
  
  Going backwards
  if we have an (a,b), that must have a come frome (a,b) came from (2a,b-a); or (a,b) came from (b-a,2a)
  case 1
  a_(n+1) = 2*a_n
  b_(n+1) = b_n - a_n
  this occurs when 2*a_n <= b_n - a_n
  3a_n <= b_n
  a_n <= 1/3 * b_n
  
  Solving for the previous (a_n, b_n),
  a_n = 1/2 * a_(n+1)
  b_n = b_(n+1) + 1/2 * a_(n+1)
  
  
  case 2
  a_(n+1) = b_n - a_n
  b_(n+1) = 2*a_n
  occurs when a_n >= 1/3 * b_n
  
  if the a_n's are distributed randomly then you'd think case 1 would occur 1/3 of the time.
  
  Looks like the 3n+1 problems???
  
      
  
  
  
  */
  
  
    
  List(5,5).zipWithIndex.combinations(2).foreach(x=>println(x))
                                                  //> List((5,0), (5,1))
  /*
  def bruteForceSolve(A: List[Int], depth: Int = 0, solutionSoFar: List[( (Int,Int) , (Int,Int) )] = List()) : Stream[List[(Int,Int)]]= {
   if (depth > 20)
    Stream()
   else
    if (A.head == 0){
     println("Solution " + solutionSoFar)
     Stream(List())
    }
    else
    {
    (
    for{
      List((a1,i1), (a2,i2))   <- A.zipWithIndex.combinations(2)
      findResult <-  bruteForceSolve(applySingleSwap(in, pairOfIndices), depth + 1, ((a1,i1),(a2,i2)) :: solutionSoFar)
     }yield List((a1,a2))
    ).toStream
    }
  } */
  
 //bruteForceSolve(A).foreach(println)
  
 def neighbourOfInput(input: List[(Int,Int)]): List[(Int,Int)] = {
  val randomIndex = scala.util.Random.nextInt(input.length)
  if (scala.util.Random.nextBoolean)
   if (scala.util.Random.nextBoolean)
    input.updated(randomIndex, ((input(randomIndex)._1 + 1               ) % A.length, input(randomIndex)._2) )
   else
    input.updated(randomIndex, ((input(randomIndex)._1 - 1 + A.length) % A.length, input(randomIndex)._2) )
  else
   if (scala.util.Random.nextBoolean)
    input.updated(randomIndex, (input(randomIndex)._1, (input(randomIndex)._2 + 1               ) % A.length) )
   else
    input.updated(randomIndex, (input(randomIndex)._1, (input(randomIndex)._2 - 1 + A.length) % A.length) )
 }                                                //> neighbourOfInput: (input: List[(Int, Int)])List[(Int, Int)]

 def applySingleSwap(in: List[Long], pairOfIndicesUnsorted: (Int,Int)): List[Long] =
 {
  if (pairOfIndicesUnsorted._1 == pairOfIndicesUnsorted._2)
   in
  else
  {
   val pairOfIndices = if (pairOfIndicesUnsorted._1 > pairOfIndicesUnsorted._2)
    (pairOfIndicesUnsorted._2,pairOfIndicesUnsorted._1)
   else
    pairOfIndicesUnsorted
   in.updated(pairOfIndices._1, 2*in(pairOfIndices._1)).updated(pairOfIndices._2, in(pairOfIndices._2)-in(pairOfIndices._1)).sorted
  }
 }                                                //> applySingleSwap: (in: List[Long], pairOfIndicesUnsorted: (Int, Int))List[Lo
                                                  //| ng]
 
 
 val x = List(3.toLong,7)                         //> x  : List[Long] = List(3, 7)
 def f(in: List[Long], count: Long): Option[List[Long]] = {
  if (count >= 1000000)
   None
  else
   if (in(0) == 0)
    Some(in)
   else
    f( List(2*in(0), in(1)-in(0)).sorted, count + 1 )
  }                                               //> f: (in: List[Long], count: Long)Option[List[Long]]
 
 f(x, 0)                                          //> res0: Option[List[Long]] = None
 f(List(3.toLong, 5),0)                           //> res1: Option[List[Long]] = None
 f(List(3.toLong, 6),0)                           //> res2: Option[List[Long]] = None
 f(List(4.toLong, 11),0)                          //> res3: Option[List[Long]] = Some(List(1, 14))
 f(List(4.toLong, 12),0)                          //> res4: Option[List[Long]] = None
 f(List(4.toLong, 13),0)                          //> res5: Option[List[Long]] = Some(List(1, 16))
 // So we've proved there will be a solution iff the sum is a power of 2.
 
 val y = for{
  x <- A.combinations(2)
 } yield f(x,0)                                   //> y  : Iterator[Option[List[Long]]] = non-empty iterator

 y.foreach(println)                               //> None
                                                  //| None
                                                  //| None

 
 def applyListOfSwaps(input: List[(Int,Int)]): List[List[Long]] = input.scanLeft(A)(applySingleSwap)
 
 def score(input : List[(Int,Int)]): Long = {
  val seriesOfAs = applyListOfSwaps(input)
  //println("series " + seriesOfAs)
  seriesOfAs.map(_.head).min
 }

 val (initialTemperature, finalTemperature, coolingRate) = (200000.0, 1.0, 0.0005)
 val scoreToBeat = 0
 
 def simulatedAnnealing(best: List[(Int,Int)], temp: Double): List[(Int,Int)] = {
  if (temp > finalTemperature) {
   val currentEnergy = score(best)
   val neighbour = neighbourOfInput( best )
   val neighbourEnergy = score(neighbour)
   //println(temp)
   if (neighbourEnergy <= scoreToBeat)
   {
    println("Solution!!!")
    println(neighbour)
    neighbour
   }
   else
   {   // Decide if we should accept the neighbour
    val acceptanceProbability = math.exp( -(neighbourEnergy - currentEnergy)/temp)
    val accept = (acceptanceProbability > math.random)
    //println("neighbourEnergy" + neighbourEnergy + "currentEnergy" + currentEnergy + "acceptProb" + acceptanceProbability)
    
    simulatedAnnealing( if (accept) neighbour else best, (1-coolingRate)*temp)
   }
  } else best
 
 }
 

 val sol = Stream.from(0).map(trial => {
 	if (trial % 1000 == 0)
 	 println(trial)
  val startingRandomSwaps = (0 until 20).map( x=> (scala.util.Random.nextInt(10),scala.util.Random.nextInt(10)) ).toList
  simulatedAnnealing(startingRandomSwaps, initialTemperature)
  }
 ).find(result => score(result) == 0).get
 println(applyListOfSwaps(sol))
 
 val SolutionTripleExclamationMark = List((6,7), (2,7), (6,8), (7,3), (6,7), (3,2), (9,8), (1,7), (0,7), (2,7), (1,3), (1,0), (0,1), (6,1), (1,2), (5,0), (7,8), (5,6), (1,5), (0,7))
 
 println(applyListOfSwaps(SolutionTripleExclamationMark))
                                                  
// Set up a tournament to eliminate them until 1 remains.
                                                  
 
}