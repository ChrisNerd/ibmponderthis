object ponder {
  /* A solution for 37 is
State(
List(
List(0, 1, 1, 1, 0, 1, 2, 1),
List(0, 1, 0, 1, 0, 0, 0, 0),
List(0, 0, 1, 0, 1, 1, 1, 2),
List(0, 0, 2, 2, 2, 0, 0, 1),
List(1, 2, 1, 0, 2, 1, 0, 0),
List(0, 2, 2, 1, 2, 0, 1, 1),
List(1, 1, 0, 0, 2, 2, 2, 1),
List(1, 1, 2, 0, 0, 0, 1, 0)))
(
Score: 37, Temperature: 0.5594704072517649)
State(List(
List(0, 1, 1, 1, 0, 1, 2, 1),
List(0, 1, 0, 1, 0, 0, 0, 0),
List(0, 0, 1, 0, 1, 1, 1, 2),
List(0, 0, 2, 2, 2, 0, 0, 1),
List(1, 2, 1, 0, 2, 1, 0, 0),
List(0, 2, 2, 1, 2, 0, 1, 1),
List(1, 1, 0, 0, 2, 2, 2, 1),
List(1, 1, 2, 0, 0, 0, 1, 0)))
(
Score: 37, Temperature: 0.5577919960300096)
State(List(
List(0, 1, 1, 1, 0, 1, 2, 1),
List(0, 1, 0, 1, 0, 0, 0, 0),
List(0, 0, 1, 0, 1, 1, 1, 2),
List(0, 0, 2, 2, 2, 0, 0, 1),
List(1, 2, 1, 0, 2, 1, 0, 0),
List(0, 2, 2, 1, 2, 0, 1, 1),
List(1, 1, 0, 0, 2, 2, 2, 1),
List(1, 1, 2, 0, 0, 0, 1, 0)))
(
Solution according to website
A(0) = [1];
A(k+1) = [A(k), A(k), A(k); A(k), A(k) + 1, A(k) + 2; A(k), A(k) + 2, A(k) + 1].

1 1 1 1 1 1 1 1 1
1 2 3 1 2 3 1 2 3
1 3 2 1 3 2 1 3 2
1 1 1 2 2 2 3 3 3
1 2 3 2 3 4 3 4 5
1 3 2 2 4 3 3 5 4
1 1 1 3 3 3 2 2 2
1 2 3 3 4 5 2 3 4
1 3 2 3 5 4 2 4 3
*/

  case class State(arr: List[List[Int]])

  def allV(n: Int): List[List[Int]] = {
    n match {
      case 0 => List(List())
      case _ => (for {
        first <- 0 to 2
        rest <- allV(n - 1)
      } yield first :: rest).toList
    }
  }

  allV(2)
  val N = 8
  val rowList = List.fill(N)(0)
  val startingState = State(List.fill(N)(rowList))

  startingState.arr(0)
  val scoreToBeat = 37

  def score(S: State) = {
    val s = allV(N) map (v =>
      S.arr map (r => (r zip v map
        (entryTuple => (entryTuple._1 + entryTuple._2) % 3)).groupBy(identity).mapValues(_.size).values.max) sum) max

    if (s < scoreToBeat)
      println(S)
    s
  }

  val sol36a = State(List(
    List(1, 1, 1, 1, 1, 1, 1, 1, 1),
    List(1, 2, 3, 1, 2, 3, 1, 2, 3),
    List(1, 3, 2, 1, 3, 2, 1, 3, 2),
    List(1, 1, 1, 2, 2, 2, 3, 3, 3),
    List(1, 2, 3, 2, 3, 4, 3, 4, 5),
    List(1, 3, 2, 2, 4, 3, 3, 5, 4),
    List(1, 1, 1, 3, 3, 3, 2, 2, 2),
    List(1, 2, 3, 3, 4, 5, 2, 3, 4),
    List(1, 3, 2, 3, 5, 4, 2, 4, 3)))
  val sol36b = State(List(
    List(1, 1, 1, 1, 1, 1, 1, 1),
    List(1, 2, 3, 1, 2, 3, 1, 2),
    List(1, 3, 2, 1, 3, 2, 1, 3),
    List(1, 1, 1, 2, 2, 2, 3, 3),
    List(1, 2, 3, 2, 3, 4, 3, 4),
    List(1, 3, 2, 2, 4, 3, 3, 5),
    List(1, 1, 1, 3, 3, 3, 2, 2),
    List(1, 2, 3, 3, 4, 5, 2, 3)))
  val sol36c = State(List(
    List(0, 0, 0, 0, 0, 0, 0, 0),
    List(0, 1, 2, 0, 1, 2, 0, 1),
    List(0, 2, 1, 0, 2, 1, 0, 2),
    List(0, 0, 0, 1, 1, 1, 2, 2),
    List(0, 1, 2, 1, 2, 0, 2, 0),
    List(0, 2, 1, 1, 0, 2, 2, 1),
    List(0, 0, 0, 2, 2, 2, 1, 1),
    List(0, 1, 2, 2, 0, 1, 1, 2)))

  val s36oaue = score(sol36c)

  

def scoreGivenNeighbour(S: State, sc: Int, r: Int, v: Int): Int = {
// Hmmm, should be something like passing in the
val s = allV(N) map (v =>
  S.arr map ( r => (r zip v map
   ( entryTuple => (entryTuple._1 + entryTuple._2 ) % 3))))
// you only need to modify the (r,v) element of each of the matrixes, do the groupBy(identity).mapValues(_.size).values.max only on the rth rows
// and compare to the previous maxs (which need to also be passed in), then modified
// Hmmm, the last step doesn't seem like it will speed things up any, but the first party does.
// Hmmm, maybe the groupBy(identity).mapValues...values.max can be sped up, by initilizating a counting array of size 3, to 0.
/*  var countarray(3).fill(0)
  ( entryTuple => (entryTuple._1 + entryTuple._2 ) % 3)).map ( countarray(_)++)
*/
// Then for i <- 0 until 8 do countingArray[ r(i) ] ++
/* if (countingArray[0] > countingARray[1] )
    if ( arr0 > arr2)
     return arr[0]
    else
     return arr[2]
  else
    if (arr1 > arr2)
     return arr[1]
    else
     return arr[2]
     
  
  max(max(arr0,arr1),arr2)
     
Hmmm, what about summing all 8 entries, will that give us unique... nope 0+2 == 1+1
s
}
*/


/* gives a list of matrices
 each with a single v applied to it
 allV(N) map (v =>
  startingState.arr map ( r => r zip v map
  ( entryTuple => (entryTuple._1 + entryTuple._2 ) % 3)
 )
)
res2: List[List[List[Int]]] = List(
List(List(0, 0, 0),
     List(0, 0, 0),
     List(0, 0, 0)),
List(List(0, 0, 1),
     List(0, 0, 1),
     List(0, 0, 1)),
....
 gives a list of matrices
we want to find the best matrix, and which v made that matrix
So for each matrix, for each row we find the count of the most frequent entry.
List(1,0,0) groupBy(identity) gives Map(1 -> List(1), 0 -> List(0,0) which doesn't look all that useful,
but we get the count of each occurance by using mapValues(._size) which returns
List(List(Map(0 -> 3),
Map(0 -> 3),
Map(0 -> 3)),
List(Map(1 -> 1, 0 -> 2),
...
We finally use "values" to select the List((3),(3),(3)), List((1,2),(1,2),(1,2)) and use "max" to return List(3,3,3), List(2,2,2).
We then sum them, to get List(9,6,...) and find the best sum possible by choosing one last max. (9)
*/
*/

val s43 = List(List(1,  2,  3,  4,  5,  6,  7,  8),
List(2,  4,  6,  8, 10, 12,  2,  4),
List(3,  6,  9, 12,  3,  6,  9, 12),
List(4,  8, 12,  4,  8, 12,  4,  8),
List(5, 10,  3,  8,  1,  6, 11,  4),
List(6, 12,  6, 12,  6, 12,  6, 12),
List(7,  2,  9,  4, 11,  6,  1,  8),
List(8,  4, 12,  8,  4, 12,  8,  4))

score(State(s43))

val s37 = State(
List(
List(0, 1, 1, 1, 0, 1, 2, 1),
List(0, 1, 0, 1, 0, 0, 0, 0),
List(0, 0, 1, 0, 1, 1, 1, 2),
List(0, 0, 2, 2, 2, 0, 0, 1),
List(1, 2, 1, 0, 2, 1, 0, 0),
List(0, 2, 2, 1, 2, 0, 1, 1),
List(1, 1, 0, 0, 2, 2, 2, 1),
List(1, 1, 2, 0, 0, 0, 1, 0)))
score(s37)
// Goal is to find a State with a score of 37 or less. Use simulated annealing to find a minimum State
def createNeighbour(s: State): State = {
 val r = util.Random.nextInt(N)
 val v = util.Random.nextInt(N)
 val inc = if (util.Random.nextBoolean()) 1 else 2
 State(s.arr.updated(r, s.arr(r).updated(v, (s.arr(r)(v) + inc) % 3)))
}

def allNeighbours(s: State) = {
 for {
  r <- 0 until N
  v <- 0 until N
  inc <- 1 to 2
 } yield State(s.arr.updated(r, s.arr(r).updated(v, (s.arr(r)(v) + inc) % 3)))
}

createNeighbour(startingState)
val (initialTemperature, finalTemperature, coolingRate) = (2.0, 0.0001, 0.005)

 
def simulatedAnnealing( best:State, temp:Double):State = {
 if (temp > finalTemperature) {
  val currentEnergy = score(best)
  val neighbour = createNeighbour( best )
  val neighbourEnergy = score(neighbour)
  if (neighbourEnergy < scoreToBeat)
  {
   println(neighbour)
   neighbour
  }
  else
  {   // Decide if we should accept the neighbour
   val accept = (math.exp((currentEnergy - neighbourEnergy)/temp) > math.random)
   simulatedAnnealing( if (accept) {
    // println("\nScore: " + neighbourEnergy, " Temperature: "+ temp)
     neighbour
   } else best, (1-coolingRate)*temp)
  }
 } else best
}

val populationSize = 40 // must be a multiple of 4

def geneticAlgorithm(pop: IndexedSeq[State], generation: Int) :IndexedSeq[State] = {
 
 // Evaluation and selection
 // Take the best half
 val bestOfPop = util.Random.shuffle( pop.sortBy{ score }.take(populationSize/2) )

 // Crossover
 // Each couple has 2 children
 val newPopulation = ((bestOfPop take populationSize/4 ) zip (bestOfPop drop populationSize/4)) flatMap { case (i1,i2) =>
 (1 to 2) map { _ => State( (i1.arr zip i2.arr ) map { case (row1,row2) =>
   (row1 zip row2) map { case (el1, el2) => if (util.Random.nextBoolean()) el1 else el2 }
  } ) } } // flatMap (x=> IndexedSeq(x,x))
 
 // Mutation
 val mutatedPopulation = newPopulation map ( ind => State( ind.arr map ( row => row map
 ( ele => if (util.Random.nextDouble() < .01) if (util.Random.nextBoolean()) (ele+1)%3 else (ele+2)%3 else ele))))
 
 
 if (generation < 100)
  geneticAlgorithm(mutatedPopulation, generation+1)
 else
  newPopulation
}
def greedy(s: State): State = {
 println("Greedy "+ score(s))
 val betterNeighbours = allNeighbours(s).filter { x => score(x) <  score(s) }
 if (betterNeighbours.isEmpty)
  s
 else
  greedy(betterNeighbours.minBy { score })
}

val allMatrices = State(): Stream[State]


def main(args:Array[String]) = {
 if (args.contains("Sim")){
  println("Starting Simulated Annealing")
  //if (false)
  // Simulated Annealing
  Stream.from(1) find { x =>
   val best = simulatedAnnealing(startingState, initialTemperature)
   var result = score(best)
   println("Last results were " + result + ". Running again. " + x )
   if (result == 37)
   {
    println("Running Greedy. Score = ")
    val gbest = greedy(best)
    result = score(gbest)
    println("Best "+ gbest + " score " + result)
   }
   result < scoreToBeat
  }
 }
 else if (args.contains("Gen"))
 {
  println("Starting Genetic Algorithm")
  Stream.from(1) find { x =>
   //Initialization
   val population = (1 to populationSize) map ( _ => State(List.fill(N)(List.fill(N)(util.Random.nextInt(3)))))
   var result = geneticAlgorithm(population, 0).map( score ).min
   println("Last results of genetic algorithm were " + result + ". Running again. " + x )
   result < scoreToBeat
  }
 }
 else
 {
  println("Starting Greedy")
  Stream.from(1) find { x =>
   //Initialization
   val bestGreedy = greedy(State(List.fill(N)(List.fill(N)(util.Random.nextInt(3)))))
   val result = score(bestGreedy)
   println("Last results of greedy algorithm were " + bestGreedy + " with score " + result +". Running again. " + x )
   result < scoreToBeat
  }
 }
}

//val matrix = Array.ofDim[Int](2,2)
val visitedStates = scala.collection.mutable.Set[State]()
def incrementRow(s: State, r: Int): State = {
  State( s.arr.take(r) ::: List(s.arr(r).map{x=> (x + 1) %3 }) ::: s.arr.drop(r+1))
}

def incrementCol(s: State, c: Int): State = {
 State( (0 until s.arr.size).toList.map { x => s.arr(x).take(c) ::: List((s.arr(x)(c) + 1)%3) ::: s.arr(x).drop(c+1) } )
}

def allActions(s:State): IndexedSeq[State] =
{
 val rows = for {
  row <- 0 until s.arr.size
 } yield incrementRow(s,row)
 rows
 
 val cols = for{
  col <- 0 until s.arr.size
 } yield incrementCol(s, col)

rows ++: cols
}

def r(s: State): IndexedSeq[State] =
{
 if (visitedStates.contains(s))
  IndexedSeq()
 else
 {
  visitedStates.add(s)
  (allActions(s).filter(!visitedStates.contains(_)) map r).flatten
 }
}

def r2(s: State): Unit =
{
 if (!visitedStates(s))
 {
  visitedStates.add(s)
  allActions(s).filter(!visitedStates.contains(_)) map r2
 }
}

r2(startingState)
visitedStates
visitedStates.size

}