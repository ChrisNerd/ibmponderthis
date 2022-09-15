object ponder {
   def time[R](block: => R): R = {
    val t0 = System.nanoTime()
    val result = block // call-by-name
    val t1 = System.nanoTime()
    val elapsedns = t1 - t0
    println("Elapsed time: " + elapsedns.toString.reverse.grouped(3).mkString(",").reverse + "ns")
    result
  }                                               //> time: [R](block: => R)R
 // The Set of 4 neighbours of a cell in 2D space. Above, Below, Left and Right.
 def neighbours(x: (Int,Int)): Set[(Int,Int)] = Set(
  (x._1 + 1, x._2),
  (x._1 - 1, x._2),
  (x._1,     x._2 + 1),
  (x._1,     x._2 - 1))                           //> neighbours: (x: (Int, Int))Set[(Int, Int)]
  
 def neighboursR(x: (Int,Int)): Set[(Int,Int)] = neighbours(x)
  .filter{ case (x,y) => y>0 || y==0 && x>=0 }    //> neighboursR: (x: (Int, Int))Set[(Int, Int)]
  
 
 // Adsorbing is like how a block falls in Tetris, when it sticks to the bottom.
 // We "adsorb" a single cell to a Set x, but we return the collection of all the possible positions
 // that we adsorb onto.
 // If x is
 // ..
 // .
 // then we'll return
 //  XX
 // X..X
 // X.X
 //  X
 // (The X values).
 def adsorb1(x: Set[(Int,Int)]): Set[(Int,Int)] = x.flatMap(neighbours) -- x
                                                  //> adsorb1: (x: Set[(Int, Int)])Set[(Int, Int)]
 
 // Make sure that all points have non-negative coordinates, and that it's as far to the bottom left as we can go.
 def normalize(x: Set[(Int,Int)]): Set[(Int,Int)] = {
  val xmin = x.minBy(_._1)._1
  val ymin = x.minBy(_._2)._2
  x.map{ case(xpoint, ypoint) => (xpoint - xmin, ypoint - ymin)}
 }                                                //> normalize: (x: Set[(Int, Int)])Set[(Int, Int)]
  
 // Given the set of all tetrominoes of size n-1 cells, create the set of all tetrominoes creatable by n cells.
 // We do this by adsorbing a single cell to each of the tetrominoes, then deduplicating (implicitly because nested for desugars to flatMap,
 // and we're doing this on a Set)
 def nextTetrominoes(setOfNMinusOneTetrominos: scala.collection.immutable.HashSet[Set[(Int,Int)]]): scala.collection.immutable.HashSet[Set[(Int,Int)]] =
 {
  println("Finding Next Set")
  time{
   for{
    tetromino <- setOfNMinusOneTetrominos
    cellToAdd <- adsorb1(tetromino)
   } yield normalize(tetromino + cellToAdd)
  }
 }                                                //> nextTetrominoes: (setOfNMinusOneTetrominos: scala.collection.immutable.Hash
                                                  //| Set[Set[(Int, Int)]])scala.collection.immutable.HashSet[Set[(Int, Int)]]
     
 // iterate is the perfect built-in function. Returns f(s), f(f(s)),...
 val allTetrominosOfSizeN = Iterator.iterate( scala.collection.immutable.HashSet(Set((0,0))) )( nextTetrominoes )
                                                  //> allTetrominosOfSizeN  : Iterator[scala.collection.immutable.HashSet[scala.c
                                                  //| ollection.immutable.Set[(Int, Int)]]] = non-empty iterator
 
 val maxN = 7                                     //> maxN  : Int = 7
 val counts = Array.fill(maxN + 1)(0L)            //> counts  : Array[Long] = Array(0, 0, 0, 0, 0, 0, 0, 0)
 val countsMutable = Array.fill(maxN + 1)(0L)     //> countsMutable  : Array[Long] = Array(0, 0, 0, 0, 0, 0, 0, 0)
 
 def coordToIndex( p: (Int,Int) ): Int = {
  val (x,y) = p
  if (x>=0)
   (x+y)*(x+y) + x
  else
   (y-x)*(y-x) + x
 }                                                //> coordToIndex: (p: (Int, Int))Int
 val poly= scala.collection.mutable.ListBuffer(): scala.collection.mutable.ListBuffer[(Int,Int)]
                                                  //> poly  : scala.collection.mutable.ListBuffer[(Int, Int)] = ListBuffer()
 
 
 def r(u: scala.collection.mutable.ListBuffer[(Int,Int)]) : Unit ={
 val oldNeighbours = (poly flatMap neighbours) ++ poly
 while (!u.isEmpty)
 {
/* println("In Mutuable")
 println("poly " + poly)
 println("u " + u)*/
  val cell = u.head
  u.remove(0)
   //val currentIndex = coordToIndex(cell)
  // val newNeighbours2 = neighboursR(cell).filter(n => coordToIndex(n) > currentIndex)
  val newNeighbours = neighboursR(cell) -- oldNeighbours
  /*if (newNeighbours2 != newNeighbours)
  { println("Neighbours differ..  cell " + cell + " poly " + poly + " oldneighbours " + oldNeighbours)
  println("newneighbours ")
  printTetromino(newNeighbours)
  println("newneighbours2 ")
  printTetromino(newNeighbours2)
  println("poly")
  printTetromino(poly.toSet)
  println("oldneighbours")
  printTetromino(oldNeighbours.toSet)
  }*/
  
  poly += cell
  if (isTetrominoOddPermutationParity(poly.toSet))
    countsMutable(poly.size) += 1
   // 4 If the size is less than P:
   // https://books.google.ca/books?id=Q8IKeoRxp7MC&pg=PA92&lpg=PA92&dq=Redelmeier%E2%80%99s+algorithm&source=bl&ots=PO5A1SERjn&sig=ACfU3U3QblqyE-6UVzbWeBSZr9qMoI5lTA&hl=en&sa=X&ved=2ahUKEwiI_NKfjPf5AhWhJH0KHRcqB68Q6AF6BAgmEAM#v=onepage&q=Redelmeier%E2%80%99s%20algorithm&f=false
   // "Step 4(a) deserves some attention. By "new neighbors" we mean only neighbors of the new cell c (chosen in Step 2) that were not neighbors of any cells of the current polyomino prior to the addition of c.
   // This ensures that we will not count the same polyomino more than once.

   // Call this algorithm recursively with the new parent being the current polyomino, and the new untried set being a copy of the current one
   if (poly.size < maxN)
   {
   // 4a add new neighbours to untried
     //u ++= newNeighbours
   // 4b recursively call child
   //val newU = u.clone()
     r(u.clone ++= newNeighbours)
     // 4c remove new neighbours from untried
     
//     u --= newNeighbours
   }
   // 5 Remove newest cell
   poly -= cell
 }
  }                                               //> r: (u: scala.collection.mutable.ListBuffer[(Int, Int)])Unit
  // The following steps are repeated until the untried set is exhausted
  // Each iteration generates a child of the parent
  // Each recursion generates all the offspring of the current child
 
 // Pretty sure we can do this whole thing with a foldLeft on untried
 /* untried.foldLeft(0)( ( totalSoFar, cell) => {
   isTetrominoOddPermutationParity(polyomino + cell)
   redelmeiyer(polyomino+cell, neighbours(cell) + untried )
  }
 )*/
 // Implementing redelmeiyer as a pure functional function
 // This function returns a vector of Int of length maxN+1, that represents the number of polyominos of formable from a given starting polyomino, and using at least 1 element from a given List of untried cells.
 // The output vector at index i is the number of polyominos of size i.
 def countPolyominos(polyomino: Set[(Int,Int)], untried: Stream[(Int,Int)]): Vector[Long] =
  /*
  println("countPolyominos " + polyomino + " untried " + untried)
  printTetromino(polyomino)
  println(polyomino.size)
  */
  
 if (untried.isEmpty)
  Vector.fill(maxN + 1)(0L)
 else
 {
  /*
  val outs = (for {
  (cell, i) <- untried.zipWithIndex
  } yield countPolyominos(polyomino + cell, untried.drop(i+1) ++  (neighboursR(cell) -- polyomino flatMap neighbours))
  ).reduce( (a,b) => a.zip(b).map{ case (c,d) => c+d  } )
  */
  val countAtThisLevel = Vector.fill(maxN + 1)(0L).updated(polyomino.size + 1, untried.count(x=>isTetrominoOddPermutationParity(polyomino + x)).toLong)
  if (polyomino.size + 1 < maxN)
  {
   //   I think we can do this with a transpose sum
   val outs = untried.zipWithIndex.map{ case (cell, i) =>
    countPolyominos(polyomino + cell, untried.drop(i+1) ++  (neighboursR(cell) -- (polyomino flatMap neighbours) -- polyomino))
   }.transpose.map(_.sum)
   countAtThisLevel.zip(outs).map{ case (c,d) => c+d  }
   // outs.toVector.updated(polyomino.size + 1, untried.count(x=>isTetrominoOddPermutationParity(polyomino + x)))
  }
  else
   countAtThisLevel
 }                                                //> countPolyominos: (polyomino: Set[(Int, Int)], untried: Stream[(Int, Int)])V
                                                  //| ector[Long]
 // Almost identical to countPolyominos(polyomino, untried) but uses a mutual array as a counter, and the neighbours are cached so they don't have to be recomputed
 // It also uses recursion instead of mapping over the untried set, which is a List here and not a Stream.
 
 
 /*
  The two basic ingredients are the polyomino, and the untried set.
  A prerequist is the the members of the untried set must be neighbours of the polyomino, and not contained within it.
  
  Say we have the polyomino
   pp
   p
   p
  
  and the untried set is some subset of its neighbours
  
   u
  pp
 up
  pu

 We are restricting adding new cells below or to the left of the bottom row. This will keep the polyomino unique (or normalized).

   u
  pp
 up
xxpu
xxxxxxxx

 Since, in this case the untried set is size 3, we are interested in the 3 polyominos that can be formed by adding them one at a time to the polyomino
   
  pp
 pp
  p
,

   p
  pp
  p
  p
,
  pp
  p
  pp

  The question is how can we recurse on these 3 polyominos without creating duplicates?
  A wrong answer is to generate all the polyominos formed with a particular member of the untried set, u', and all the polyominos without that member.
  These two sets will be all the polyominos formable but we will over-count.
  For example, if we have the 3 untried cells a, b, c. We will form the power set, {null} {a} {b} {c} {ab} {ac} {bc} {abc}, but {abc} will be gotten to by adding a first, b first or c first.
  The correct answer is to sort the untried cells and only allow them to be added in their sorted order. If we add b first, we can't subsequently add a. If we add a first, we can still add b later on.
  There's exactly 1 way to add {abc}, which is add a, then b, then c. So when we add b, we must eliminate a from the untried set.
  It's basically enumerating combinations and not permutations.
  There are a bunch of papers on enumerating combinations, having to do with Grey Codes and Hamiltonian Paths. But we'll use a simple recursive one.
  When we grow the polyomino by 1, we want to expand the untried set with the new neighbours. We must not add pre-existing neighbours,
  since they would have already been added to the untried set when that element was added. We can't have an element in the untried set twice, I mean - it's a Set.
  
  Another way to think of it is say we are able to assign a unique index to every cell coordinate... then for a given polyomino there is exactly one way to place those cells in order.
  So when that polyomino was generated it must have been built up in only one way.
  One such unique assignments to coordinates is coordToIndex.
  But that gives incorrect results... We also have to filter out oldneighbours to make sure we're not double adding.
  
  
  So the key to this recursion is calling
  redelmeiyer(polyomino + cell,  (neighboursR(cell) -- neighboursOfPolyomino -- polyomino).toList ++ untriedTail , neighboursOfPolyomino ++ neighboursR(cell) )
 */
 
 
 
 
 def redelmeiyer(polyomino: Set[(Int,Int)], untried: List[(Int,Int)], neighboursOfPolyomino: Set[(Int,Int)] = Set()): Unit =
 
 untried match {
  // could also do it like
  /*
  untried.zipWithIndex.map{ case (cell, i) => {
   
   if (isTetrominoOddPermutationParity(polyomino + cell))
    counts(polyomino.size + 1) += 1
   if (polyomino.size + 1 < maxN)
    redelmeiyer(polyomino + cell,  (neighboursR(cell) -- neighboursOfPolyomino  -- polyomino).toList ++ untried.drop(i+1), neighboursOfPolyomino ++ neighboursR(cell) )
   }
  }
  */
  
  case Nil => ()
  // 1. "Remove" an arbitry element from unTried.
  // This is done with a linked list, which he calls a stack. Removing the first leaving a tail
  case cell :: untriedTail =>
  {
   // 2. Place a cell at this point
   // 3. Count this new polyomino
   if (isTetrominoOddPermutationParity(polyomino + cell))
    counts(polyomino.size + 1) += 1
   // 4 If the size is less than P:
   // 4a add new neighbours to untried
   // https://books.google.ca/books?id=Q8IKeoRxp7MC&pg=PA92&lpg=PA92&dq=Redelmeier%E2%80%99s+algorithm&source=bl&ots=PO5A1SERjn&sig=ACfU3U3QblqyE-6UVzbWeBSZr9qMoI5lTA&hl=en&sa=X&ved=2ahUKEwiI_NKfjPf5AhWhJH0KHRcqB68Q6AF6BAgmEAM#v=onepage&q=Redelmeier%E2%80%99s%20algorithm&f=false
   // "Step 4(a) deserves some attention. By "new neighbors" we mean only neighbors of the new cell c (chosen in Step 2) that were not neighbors of any cells of the current polyomino prior to the addition of c.
   // This ensures that we will not count the same polyomino more than once.

   // 4b recursively call child
   // Call this algorithm recursively with the new parent being the current polyomino, and the new untried set being a copy of the current one
   if (polyomino.size + 1 < maxN)
    // could avoid the O(N) operation of mapping the neighbours each time. Just add them to a Set.
    // There was a very subtle bug in the following line. I didn't have the "-- polyomino" because I thought the whole polyomino would be included
    // in the flatMap neighours.
    // It turns out we miss the case when n=1, we have a single celled polyomino. Its flatMap neighbours doesn't include itself.
    // So we wind up with the case where "cell" has a neighbour to the single-celled polyomino and it's not removed by the flatMap.
//    redelmeiyer(polyomino + cell, untriedTail ++ (neighboursR(cell) -- (polyomino flatMap neighbours) -- polyomino ) )
     redelmeiyer(polyomino + cell,  (neighboursR(cell) -- neighboursOfPolyomino -- polyomino).toList ++ untriedTail , neighboursOfPolyomino ++ neighboursR(cell) )
   // 4c remove new neighbours from untried
   // 5 Remove newest cell
   // (no need to remove anything since it's all immutable
   redelmeiyer(polyomino, untriedTail, neighboursOfPolyomino)
  }
 }                                                //> redelmeiyer: (polyomino: Set[(Int, Int)], untried: List[(Int, Int)], neigh
                                                  //| boursOfPolyomino: Set[(Int, Int)])Unit
 
 /*
   f9
  e84a
 d7315b
    026c
 OK, it looks like the square numbers, 0, 1, 4, 9, 16 are above the origin along the positive y axis.
 So we can get the indecies of the first quadrant.
 (x,y) projects up and to the left to intersect with the y axis.
 y+x. Take that number and square it to get the index of the y intersecpt. Then count off the x increments.
 So I= (x+y)^2+x.
 Lets see (0,0) => 0, good
 (0,1)=>1
 (0,a)=a^2, good.
 (1,0)=>2 good.
 (1,1)=>5 good.
 
 For the second quadrant, project it up and to the right to get the y intersept and then subtrack off x coordinates from there.
 (y-x)^2 + x.
 (-1,1) => 3 good.
 
 
 
 
 
 
 
 
 
 
   */
 //allTetrominosOfSizeN.take(5).foreach(println)
 
 def printTetromino(tetromino: Set[(Int,Int)]) = tetromino.size match {
  case 0 => Nil
  case _ => {
   val xmin = tetromino.minBy(_._1)._1
   val xmax = tetromino.maxBy(_._1)._1
   val ymin = tetromino.minBy(_._2)._2
   val ymax = tetromino.maxBy(_._2)._2
   (ymax to ymin by -1).foreach(y =>
    println( (xmin to xmax).map( x => if (tetromino( (x,y) ) ) 'x' else ' ').mkString)
   )
   println("------------")
  }
 }                                                //> printTetromino: (tetromino: Set[(Int, Int)])Any
 
 /*allTetrominosOfSizeN.zipWithIndex.take(5).foreach{s =>
  println(s._2)
  s._1.foreach(printTetromino)
 }*/
 
 // the bottom left is (0,0)
 // the top left is (0, ymax)
 // the top right is (xmax,ymax)
 // the bottom right is (xmax, 0)
 // This order is like reading
 // We define it as returns true if a < b
 // true iff a is above b, or if they are at the same height (y coordinate), than if a is to the left of b.
 def sortOrderLeftToRightTopToBottom(a: ((Int,Int),Int), b: ((Int,Int),Int)): Boolean = a._1._2 > b._1._2 || a._1._2 == b._1._2 && a._1._1 < b._1._1
                                                  //> sortOrderLeftToRightTopToBottom: (a: ((Int, Int), Int), b: ((Int, Int), In
                                                  //| t))Boolean
 
 def sortOrderTopToBottomLeftToRight(a: ((Int,Int),Int), b: ((Int,Int),Int)): Boolean = a._1._1 < b._1._1 || a._1._1 == b._1._1 && a._1._2 < b._1._2
                                                  //> sortOrderTopToBottomLeftToRight: (a: ((Int, Int), Int), b: ((Int, Int), In
                                                  //| t))Boolean


 def followCycle(x: Map[Int,Int], target: Int, current: Int): List[Int] =
 {
  val next = x(current)
  if (next == target)
   List(current)
  else
   current :: followCycle(x, target, next)
 }                                                //> followCycle: (x: Map[Int,Int], target: Int, current: Int)List[Int]

 def decomposeIntoCycles(x: Map[Int,Int]): List[List[Int]] =
 {
  if (x.isEmpty)
   List()
  else
   {
    val starting: Int = x.head._1
    val currentCycle = followCycle(x, starting, starting)
    currentCycle :: decomposeIntoCycles(x -- currentCycle)
   }
  }                                               //> decomposeIntoCycles: (x: Map[Int,Int])List[List[Int]]
 // True for odd parity permutation
 // Odd parity permutations cycles have an even number of elements in their orbit, and vice versa.
 // You can think of it as it takes n-1 swaps to effect a cycling of length n.
 // n-1 elements are in the wrong place, so each needs a swap to be put in the write place. The last one has no choice but to be in the right place.
 // So we're looking for an odd number of even-sized cycles.
 def parityOfMapOfIntsToInts(x: Map[Int,Int]): Boolean = decomposeIntoCycles(x).count( _.size % 2 == 0 ) % 2 != 0
                                                  //> parityOfMapOfIntsToInts: (x: Map[Int,Int])Boolean

 def parityOfListOfInts(x: List[Int]): Boolean =  parityOfMapOfIntsToInts(x.zipWithIndex.toMap)
                                                  //> parityOfListOfInts: (x: List[Int])Boolean
 
 def isTetrominoOddPermutationParity(x: Set[(Int,Int)]): Boolean = {
  val xList = x.toList.zipWithIndex
  val xSorted1 = xList.sortWith(sortOrderTopToBottomLeftToRight).map(_._2)
  val xSorted2 = xList.sortWith(sortOrderLeftToRightTopToBottom).map(_._2)
  parityOfListOfInts(xSorted1) != parityOfListOfInts(xSorted2)
 }                                                //> isTetrominoOddPermutationParity: (x: Set[(Int, Int)])Boolean

 /*val streamSolution = allTetrominosOfSizeN.map{ t =>
  println("Counting")
  time{
   t.count(isTetrominoOddPermutationParity)
  }
 }.foreach(println)
*/

 def main(args: Array[String]): Unit = {
 println("Starting purely mutuable")
 time{ r(scala.collection.mutable.ListBuffer((0,0))) }
 countsMutable.zipWithIndex.foreach(println)
 
  println("Starting pure functional")
  time{
   val pureFunctional = countPolyominos(Set(), Stream((0,0)))
   pureFunctional.zipWithIndex.foreach(println)
  }
  println("Done")


  println("Starting Redelmeyer")
  time{
   redelmeiyer(Set(), List((0,0)))
   counts.zipWithIndex.foreach(println)
  }
  println("Done")
  
  
  val solution = allTetrominosOfSizeN.zipWithIndex.map{ case(setOfTetrominos, n) =>
   println("Counting")
   time{
    (n, setOfTetrominos.count(isTetrominoOddPermutationParity))
   }
  }.take(maxN).foreach(println)
 }                                                //> main: (args: Array[String])Unit
 // 0,0,1,3,11,35,108,380,1348,5014,18223,67634,252849,950346,3602437 (which is n=14), crashes on n=15
 // java.lang.OutOfMemoryError: Java heap space
 //       at scala.collection.immutable.HashSetBuilder.insertValue(HashSet.scala:1975)
 //... at ponder$.normalize(ponder.sc:14)
 /*
 Finding Next Set
Elapsed time: 17,040,623,026ns
Counting
Elapsed time: 8,656,363,269ns
(11,252849)
Finding Next Set
Elapsed time: 71,326,749,125ns
Counting
Elapsed time: 33,215,554,951ns
(12,950346)
Finding Next Set
Elapsed time: 835,092,980,322ns
Counting
Elapsed time: 179,328,601,945ns
(13,3602437)
Finding Next Set
Exception in thread "main" java.lang.OutOfMemoryError: Java heap space
        at scala.collection.immutable.HashSetBuilder.insertElement(HashSet.scala:1962)
        at scala.collection.immutable.HashSetBuilder.insertValue(HashSet.scala:1982)
        at scala.collection.immutable.HashSetBuilder.update(HashSet.scala:2025)
        at scala.collection.immutable.HashSetBuilder.addOne(HashSet.scala:2062)
        at scala.collection.immutable.HashSetBuilder.addAll(HashSet.scala:2085)
        at scala.collection.immutable.HashSetBuilder.addAll(HashSet.scala:1942)
        at scala.collection.StrictOptimizedIterableOps.flatMap(StrictOptimizedIterableOps.scala:117)
        at scala.collection.StrictOptimizedIterableOps.flatMap$(StrictOptimizedIterableOps.scala:104)
        at scala.collection.immutable.HashSet.flatMap(HashSet.scala:34)
        at ponder$.adsorb1(ponder.sc:29)
real    131m59.623s
        
 */
}