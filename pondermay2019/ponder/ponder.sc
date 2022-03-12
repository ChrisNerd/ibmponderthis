import scala.collection.immutable.Queue
import scala.collection.mutable.PriorityQueue
import java.util.function.ToLongFunction
  
object ponder {
   val n = 8
   type Coast = List[Boolean] // size n
   def allCoasts(i : Int): Iterator[List[Boolean]] = i match {
    case 0 => Iterator(List())//Iterator(List(false,true))
    case _ =>
     for{
      first <- Iterator(false,true)
      rest <- allCoasts(i-1)
     } yield (first :: rest)
   }
   
   allCoasts(3)
   allCoasts(3).foreach(println)
   
   
    
   case class State(s: Coast, side: Boolean) // s is of size n. side is true if we (the boat) are on side A.
   val startingState: State = State(List.fill(n)(true), true)
   val solutionState: State = State(List.fill(n)(true), false)
    
  val exampleAllowableCoasts: Set[List[Boolean]] = Set( List.fill(n-1)(true) // Definitely need to allow the state of everyone being on one side.
  // This is both the start and end coast. But the "state" would include the side boolean.
  // Each "coast List" is of size n-1 because we're only considering the side with person index 0.
  ,List(true,true,false,false,false),
  List(true,false,true,true,true),
  List(true,false,true,true,false),
  List(true,false,false,false,false),
  List(false,true,true,true,true),
  List(false,true,true,false,true),
  List(false,true,false,false,false),
  List(false,false,true,true,true),
  List(false,false,true,false,false),
  List(false,false,false,false,false)
  
  )  // It's complements must also be in the allowableCoasts
  
  exampleAllowableCoasts.map(a => a.zipWithIndex.map{ case(b,i) => if (b) scala.math.pow(2,n-i-2) else 0}.sum).toList.map(_.toInt).sorted.reverse
  
  def booleanRepFromInt(i: Int) : List[Boolean] = i match {
   case 0 => List()
   case a => (if(a%2 == 0) false else true) :: booleanRepFromInt(a/2)
  }
  
  booleanRepFromInt(15)
  booleanRepFromInt(16)
  booleanRepFromInt(17)
  booleanRepFromInt(18)
  
  
  
  
  
  def isAllowable(allowableCoasts: Set[List[Boolean]])(c: Coast) = allowableCoasts.contains(c.tail.map( _ == c.head))
  startingState.s
  startingState.s.tail
  startingState.s.tail.map( _ ^ startingState.s.head)
  startingState.s.tail.map( _ ^ !startingState.s.head)
  startingState.s.tail.map( _ == startingState.s.head)
  
   
//  isAllowable(startingState.s)
//  isAllowable(solutionState.s)
  
  // I don't think we'll need this function...
  def oppositeCoast( c: Coast ) = c.map( ! _ )
  oppositeCoast(startingState.s)
  oppositeCoast(solutionState.s)

  // Returns the new coast (the destination of the boat).
  // Boat will be a set of either 1 or 2 elements (Ints) containing the indexes of the passengers.
  // Requires that the boat passengers exists on coast c. ... assert
  // So the boat people will definitely be on the new coast now (true), the rest will be the people not on coast c: !a
  def moveBoatToOppositeCoast( c: State, boat: Set[Int] ) = {
   assert( boat.forall(p => c.s(p)))
   State(c.s.zipWithIndex.map{ case (a,i) => if (boat.contains(i)) true else !a } , !c.side)
  }

  moveBoatToOppositeCoast(startingState, Set(3,4))
  moveBoatToOppositeCoast(startingState, Set(3))
  moveBoatToOppositeCoast(solutionState, Set(3))
  
  def generateSetOfPossibleBoats(allowableCoasts: Set[List[Boolean]])(currentState: State): Iterator[Set[Int]] = {
   assert(isAllowable(allowableCoasts)(currentState.s))
   val indexesOfPeopleOnShore = currentState.s.zipWithIndex.filter( x => x._1 ).map(_._2)
   val possibleBoats = indexesOfPeopleOnShore.toSet.subsets(2) ++ indexesOfPeopleOnShore.toSet.subsets(1)
   possibleBoats.filter( b => isAllowable(allowableCoasts)(moveBoatToOppositeCoast(currentState, b).s))
  }
  
//  generateSetOfPossibleBoats(startingState).foreach(println)
  
  /*
  So the general algorithm I'm going for is to start with a dictionary of allowableCoasts, which, at the end will be our solution.
  We'll do a depth first traversal trying to solve with allowableCoasts. 3 results are possible:
  1. We find a solution less than 73 moves
  2. We find a solution greater than 73, in which case we ensure that we've found the shortest solution.
  3. We find that there's no solution.
  
  If we are in case 2, we've solved the problem!
  If we are in case 1, we need to remove an item from allowableCoasts and repeat.
  If we are in case 3, we need to add an item from allowableCoasts and repeat.
  
  The main loop therefore will be to systematically add and remove entries from allowableCoasts. That's the question isn't it?!
  At the risk of getting ahead of ourselves, can we add and remove entries while we're doing the search?
  
  The search itself should inform which entries are crutial?
  
  Maybe the answer is something like the Tower's of Hanoi.
  
  The search must be recursive, building a data structure while we go so we don't visit a state we've already been to.
  something like
  */
  
  List(0,1).minBy(x => x)
//  List().minBy(x => x) will give error
  
  // What we really want is 2 booleans answered.
  // Is this solvable? and if so, is it solvable in under 73 moves.
  // We don't need the solution. We don't need the shortest path necessarily.
  // We can get a subobtimal path that's less than 73 moves.
  // If we find a path that's greater than 73, then we need prove we've found the shortest path.
  


  def generateQueueOfNewStates(allowableCoasts: Set[List[Boolean]])(statesVisitedSoFar: Set[State])( s: State): Queue[State] =
   {
   val it = generateSetOfPossibleBoats(allowableCoasts)(s).flatMap(boat => {
   val p = moveBoatToOppositeCoast(s, boat)
   if (statesVisitedSoFar.contains(p))
    None
   else
   Some(p)
   }
  )
   Queue(it.toSeq: _*)
   }
// generateQueueOfNewStates(Set())(startingState)

/*1
  procedure BFS(G,start_v):
2      let S be a queue
3      S.enqueue(start_v)
4      while S is not empty
5          v = S.dequeue()
6          if v is the goal:
7              return v
8          for all edges from v to w in G.adjacentEdges(v) do
9              if w is not labeled as discovered:
10                 label w as discovered
11                 w.parent = v
12                 S.enqueue(w)
*/
  
  def BreadthFirstSearchDistanceToEnd(allowableCoasts: Set[List[Boolean]])(startingState: State, solutionState: State): Option[Int] = {
   val distancesFromStart = scala.collection.mutable.Map( startingState -> 0)
   val distancesFromEnd =   scala.collection.mutable.Map( solutionState -> 0)
   val Q = scala.collection.mutable.Queue(startingState)
   while (! Q.isEmpty && ! distancesFromStart.contains(solutionState))
   {
    val v = Q.dequeue
    if (v == solutionState)
     Some(distancesFromStart(v))
    else
     generateQueueOfNewStates(allowableCoasts)(distancesFromStart.keys.toSet)(v).foreach{
      w =>
       distancesFromStart.getOrElseUpdate(w, distancesFromStart(v) + 1)
       Q.enqueue(w)
    }
   }
   distancesFromStart.get(solutionState)
  }
  
//  BreadthFirstSearchDistanceToEnd(startingState, solutionState)

  
  
  
  def findPathDFS( currentState : State, statesVisitedSoFar: List[State], allowableCoasts: Set[List[Boolean]] ): Option[List[State]] =
  {
   if (currentState.s.count(_ == true) <= 2 && currentState.side)
    Some(solutionState :: currentState :: statesVisitedSoFar) // prepending for O(1) vis appending for O(n)
   else
    // maybe have to use withFilter( statesVisitedSoFar +
    // Need to look up how to do a shortest path solver using a depth first search.
    // Ah, use minBy instead of find.
     generateSetOfPossibleBoats(allowableCoasts)(currentState).filterNot(boat => statesVisitedSoFar.contains(moveBoatToOppositeCoast(currentState, boat)))
     // Here's where we should do a sort, to implement A*
      .find( peopleInBoat =>
{
        // This could crash it!!! println( currentState, peopleInBoat, statesVisitedSoFar.size )
        findPathDFS(
         moveBoatToOppositeCoast(currentState, peopleInBoat), currentState :: statesVisitedSoFar, allowableCoasts) match
         {
          case Some(p) => true
          case None => false
         }}
       ) match
     {
      case Some(peopleInBoat) => findPathDFS(
         moveBoatToOppositeCoast(currentState, peopleInBoat), currentState :: statesVisitedSoFar, allowableCoasts)
      case None => None
     }
  }
  
//  findPathDFS(startingState, List(), allowableCoasts)
  
//  findPathDFS(startingState, List(), allowableCoasts).get.size

  
  // tail recursive
  
 /* val coastsWeveTried: List[Coast] = List()
  def tryCoasts( allowableCoasts: Set[List[Boolean]]) : Option[Set[List[Coast]]] = {
   BreadthFirstSearchDistanceToEnd(allowableCoasts)(startingState, solutionState) match {
    case Some(shortestPathLength) if shortestPathLength < 19 =>
    // Too short, therefore too easy. Need to remove a coast from allowable
    {
     allowableCoasts.find{ coast => !allowableCoasts.contains(allowableCoasts - coast)) match
      {
       case Some(coast) => println("removing a coast, solved it in", shortestPathLength) ;
       {
        tryCoasts(allowableCoasts - coast) match
        {
         case Some(a) => true
         case _ => false
        }
       }
       }
       case None => {
        println("We've removed all available coasts!!!")
        None
       }
      }
     }
    case Some(shortestPathLength) => { println("Solution Found!!! Solved it in ", shortestPathLength, " num rules ", allowableCoasts.size)
    println(allowableCoasts)
    None
    }
    case None => // Too hard, need to add a coast that doesn't yet exist in allowableCoasts
    {
      allCoasts(n).find( coast => allowableCoasts.contains(allowableCoasts + coast)) match
      {
       case Some(coast) => tryCoasts(allowableCoasts + coast)
       case None => {
        println("We're unable to add a new coast!!!", allowableCoasts.size)
        None
       }
      }
    }
   }
  }
 allCoasts(n).size
 val try1 = Set(List.fill(n-1)(true) ,List(true,true,false,false,false),
  List(true,false,true,true,true),
  List(true,false,true,true,false),
  List(true,false,false,false,false),
  List(false,true,true,true,true),
  List(false,true,true,false,true),
  List(false,true,false,false,false),
  List(false,false,true,true,true),
  List(false,false,true,false,false),
  List(false,false,false,false,false)
  
  )
 
  tryCoasts(try1) // Definitely need to allow the state of everyone being on one side.
  */
  
  
  // BreadthFirstSearchDistanceToEnd(startingState, solutionState)
/*  findPathDFS( startingState, List(), allowableCoasts) match {
   case Some(sol) if sol.size <= 19 => {
    allowableCoasts.find(coast => !coastsWeveTried.contains(allowableCoasts - coast)) match
    {
     case Some(coast) => findPathDFS(startingState, List(), allowableCoasts - coast)   // Too short, therefore too easy
     case None => {
      println("We've removed all available coasts!!!")
      None
    }
    }
   }
   case Some(sol) /*if sol.size >= 73 */ =>  println("Solution Found!!!", allowableCoasts) //
   case None =>  allowableCoasts.find(coast => !coastsWeveTried.contains(allowableCoasts ++ this doesn't make any sense coast)) match
   {
    case Some(coast) => findPathDFS(startingState, List(), allowableCoasts + coast)   // Too restrictive.
    case None => {
     println("We're unable to add a new coast!!!")
     None
    }
   }
  }
  */
  
  def score(allowedC : Set[List[Boolean]]) = 13 - BreadthFirstSearchDistanceToEnd(allowedC)(startingState, solutionState).getOrElse(8)
  
  def intToBoolList(i: Int, len: Int): List[Boolean] = len match
  {
   case 0 => List()
   case a => (if (a%2==0) false else true) :: intToBoolList(a/2, len-1)
  
  }
  // Will have 128 neighbours to choose from
  // Choose a number from 0 to 126 (not 127 because we need that in there)
  // create a List[Boolean] of length 7 from that int.
  // If it's in allowedC, remove it. Else add it.
    def createNeighbour( allowedC: Set[List[Boolean]]) =   {
    val i=  scala.util.Random.nextInt(math.pow(2,n-1).toInt -1 )
  val booli = intToBoolList(i, 7)
  if (allowedC.contains(booli))
   allowedC - booli
   else
   allowedC + booli
   }
  
  val (initialTemperature, finalTemperature, coolingRate) = (20.0, 0.05, 0.00005)

  def simulatedAnnealing(best: Set[List[Boolean]], temp: Double): ( Set[List[Boolean]], Int) = {
    if (temp > finalTemperature) {
      val currentEnergy = score(best)
      val neighbour = createNeighbour(best)
        // println("curr neigh", best, neighbour)
      val neighbourEnergy = score(neighbour)
      //println("curr neigh temp", currentEnergy, neighbourEnergy, temp)
      if (neighbourEnergy <= 0) {
        println(neighbour)
        (neighbour,neighbourEnergy)
      } else { // Decide if we should accept the neighbour
        val accept = (math.exp((1+currentEnergy - neighbourEnergy) / temp) > math.random)
        simulatedAnnealing(if (accept) {
 //          println("\nScore: " + neighbourEnergy, " Temperature: "+ temp, "Numberof Rules", neighbour.size)
          neighbour
        } else best, (1 - coolingRate) * temp)
      }
    } else (best,score(best))
  }
  
 val outS = simulatedAnnealing(exampleAllowableCoasts, initialTemperature)
                                                  
 outS == exampleAllowableCoasts
 outS
 val blah = 5

  List(11).find{ ss =>

   (0 to 30).toSet.subsets(ss).find{ allowCInt =>  {
    val allowCList :Set[List[Boolean]] = (allowCInt + 31).map(i => I think there's a bug here, this function should return a list of length n-1 booleanRepFromInt(i) )
    BreadthFirstSearchDistanceToEnd(allowCList)(startingState, solutionState).getOrElse(0) >= 73 //19
   }
   }
    match {
   case Some(a) => println("Solution Found!!!", a); true
   case _ => false
  }
}
 def main()
 {
   (0 to 30).toSet.subsets(7).find{ allowCInt =>  {
   val allowCList :Set[List[Boolean]] = (allowCInt + 31).map(i => booleanRepFromInt(i) )
   BreadthFirstSearchDistanceToEnd(allowCList)(startingState, solutionState).getOrElse(0) >= 19
   }
  }
 
  //tryCoasts(Set(List.fill(n-1)(true)))
 }
}