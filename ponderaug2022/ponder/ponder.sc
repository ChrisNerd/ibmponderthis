object ponder {
  /*
 https://research.ibm.com/haifa/ponderthis/challenges/August2022.html
 A dance show consists of four dancers, designated A, B, C, and D, who dance on a stage for n units of time.

 Either a single dancer performs in the middle section, or two dancers dance on the right and left parts of the stage (one on each side).

 Rule 1 - When on the sides of the stage, each dancer can dance for 1, 2, or 3 units of time before being replaced by a dancer currently not on the stage.

 Rule 2 - A dancer in the middle section must be replaced in 1 unit of time, either by another dancer in the middle or two others in the side sections.

 Rule 3 - Dancers cannot switch places on stage directly from one unit of time to the next.

 Rule 4 - If two dancers need to be replaced at the same time, they must be replaced by a single dancer in the middle.

 The following arrangement is illegal because B,C start together after A,D finish; A dances on the left side and suddenly switches to the middle; and B,D switch sides.
AABBBADB
BDCCAABD

 There are 16 ways to perform a n=1 dance, 120 ways to perform an n=2 dance, and 17,342,172 ways to perform an n=8 dance.
 Since numbers grow rapidly, we compute them modulo N=3141592653 . For n=128 , the number of ways modulo N is 2,484,449,895.

 Your goal: Compute the number of ways for n=2^{24}=16,777,216 modulo N=3141592653

 A Bonus "*" will be given for computing the number of ways for n=2^{256} modulo N=3141592653

 For n=1
 there are 4 perm 2 = 4*3 = 12 ways of getting 2 people on the sides. + 4 ways of getting a single person in the middle. 12+4=16.

 For n=2
 We transition from any of the 16 states to some of the 16 other states. So we can do a 16x16 matrix eleminating invalid transitions.
 */

  case class State(left: Char, right: Char)
  type Routine = List[State]
  val generate16States: List[State] = {
    val dancers = ('A' to 'D').toList
    val dancersOnSides = dancers.combinations(2).map(x => State(x(0), x(1))).toList
    val dancersOnSidesReversed = dancersOnSides.map(x => State(x.right, x.left))
    val dancersInMiddle = dancers.map(x => State(x, x))

    dancersInMiddle ::: dancersOnSides ::: dancersOnSidesReversed
  }                                               //> generate16States  : List[ponder.State] = List(State(A,A), State(B,B), State
                                                  //| (C,C), State(D,D), State(A,B), State(A,C), State(A,D), State(B,C), State(B,
                                                  //| D), State(C,D), State(B,A), State(C,A), State(D,A), State(C,B), State(D,B),
                                                  //|  State(D,C))
  generate16States.size                           //> res0: Int = 16

  // doesn't apply until we get to n=4
  // Rule 1 - When on the sides of the stage, each dancer can dance for 1, 2, or 3 units of time before being replaced by a dancer currently not on the stage.
  def doesntViolateRule1(routine: Routine) = {
    routine.size < 4 ||
      routine.map(_.left).distinct.size >= 2 && routine.map(_.right).distinct.size >= 2
  }                                               //> doesntViolateRule1: (routine: ponder.Routine)Boolean
  // Rule 2 - A dancer in the middle section must be replaced in 1 unit of time, either by another dancer in the middle or two others in the side sections.
  def doesntViolateRule2(firstState: State, secondState: State) = {
    // are they all the same?
    !(firstState.left == firstState.right && firstState.left == secondState.left && firstState.left == secondState.right)
  }                                               //> doesntViolateRule2: (firstState: ponder.State, secondState: ponder.State)Bo
                                                  //| olean

  // Rule 3 - Dancers cannot switch places on stage directly from one unit of time to the next.
  def doesntViolateRule3(firstState: State, secondState: State) = {
    firstState == secondState ||
      // allow for a middle dance to be replaced by another middle dancer
      firstState != secondState && firstState.left == firstState.right && secondState.left == secondState.right ||
      // if the two states differ then they must contain at least 3 dancers total
      firstState != secondState && Set(firstState.left, firstState.right, secondState.left, secondState.right).size >= 3
  }                                               //> doesntViolateRule3: (firstState: ponder.State, secondState: ponder.State)Bo
                                                  //| olean

  // Rule 4 - If two dancers need to be replaced at the same time, they must be replaced by a single dancer in the middle.
  def doesntViolateRule4(firstState: State, secondState: State) = {
    firstState.left == firstState.right || // rule doesn't apply when we start with a dancer in the middle
      firstState == secondState || // also don't worry about it if there are no replacements
      !(firstState.left != secondState.left && firstState.right != secondState.right && secondState.left != secondState.right)
  }                                               //> doesntViolateRule4: (firstState: ponder.State, secondState: ponder.State)Bo
                                                  //| olean
  def doesntViolateAnyRules(statesSoFar: Routine, stateToAppend: State) = {
    val firstState = statesSoFar.last
    doesntViolateRule1(statesSoFar.drop(statesSoFar.size - 3) :+ stateToAppend) &&
      doesntViolateRule2(firstState, stateToAppend) &&
      doesntViolateRule3(firstState, stateToAppend) &&
      doesntViolateRule4(firstState, stateToAppend)
  }                                               //> doesntViolateAnyRules: (statesSoFar: ponder.Routine, stateToAppend: ponder.
                                                  //| State)Boolean

  doesntViolateRule2(State('A', 'A'), State('A', 'A'))
                                                  //> res1: Boolean = false
  doesntViolateRule3(State('A', 'A'), State('A', 'A'))
                                                  //> res2: Boolean = true
  doesntViolateRule4(State('A', 'A'), State('A', 'A'))
                                                  //> res3: Boolean = true

  doesntViolateRule2(State('A', 'A'), State('B', 'B'))
                                                  //> res4: Boolean = true
  doesntViolateRule3(State('A', 'A'), State('B', 'B'))
                                                  //> res5: Boolean = true
  doesntViolateRule4(State('A', 'A'), State('B', 'B'))
                                                  //> res6: Boolean = true

  val allnEquals2Routines: List[Routine] = {
    for {
      firstState <- generate16States
      secondState <- generate16States
      if (
        //  doesntViolateRule1(firstState, secondState) &&
        doesntViolateRule2(firstState, secondState) &&
        doesntViolateRule3(firstState, secondState) &&
        doesntViolateRule4(firstState, secondState))
    } yield List(firstState, secondState)         //> allnEquals2Routines  : List[ponder.Routine] = List(List(State(A,A), State(B
                                                  //| ,B)), List(State(A,A), State(C,C)), List(State(A,A), State(D,D)), List(Stat
                                                  //| e(A,A), State(B,C)), List(State(A,A), State(B,D)), List(State(A,A), State(C
                                                  //| ,D)), List(State(A,A), State(C,B)), List(State(A,A), State(D,B)), List(Stat
                                                  //| e(A,A), State(D,C)), List(State(B,B), State(A,A)), List(State(B,B), State(C
                                                  //| ,C)), List(State(B,B), State(D,D)), List(State(B,B), State(A,C)), List(Stat
                                                  //| e(B,B), State(A,D)), List(State(B,B), State(C,D)), List(State(B,B), State(C
                                                  //| ,A)), List(State(B,B), State(D,A)), List(State(B,B), State(D,C)), List(Stat
                                                  //| e(C,C), State(A,A)), List(State(C,C), State(B,B)), List(State(C,C), State(D
                                                  //| ,D)), List(State(C,C), State(A,B)), List(State(C,C), State(A,D)), List(Stat
                                                  //| e(C,C), State(B,D)), List(State(C,C), State(B,A)), List(State(C,C), State(D
                                                  //| ,A)), List(State(C,C), State(D,B)), List(State(D,D), State(A,A)), List(Stat
                                                  //| e(D,D), State(B,B)), List(State(D,D), State(C,C)), List(State(D,D), State(A
                                                  //| ,B)), List(State(D,D), State(A,C)), List(State(D,D), State(B,C)), List(Stat
                                                  //| e(D,D), State(B,A)), List(State(D,D), State(C,A)), List(State(D,D), State(C
                                                  //| ,B)), List(State(A,B), State(C,C)), List(State(A,B), State(D,D)), List(Stat
                                                  //| e(A,B), State(A,B)), List(State(A,B), State(A,C)), List(State(A,B), State(A
                                                  //| ,D)), List(State(A,B), State(C,B)), List(State(A,B), State(D,B)), List(Stat
                                                  //| e(A,C), State(B,B)), List(State(A,C), State(D,D)), List(State(A,C), State(A
                                                  //| ,B)), List(State(A,C), State(A,C)), List(State(A,C), State(A,D)), List(Stat
                                                  //| e(A,C), State(B,C)), List(State(A,C), State(D,C)), List(State(A,D), State(B
                                                  //| ,B)), List(State(A,D), State(C,C)), List(State(A,D), State(A,B)), List(Stat
                                                  //| e(A,D), State(A,C)), List(State(A,D), State(A,D)), List(State(A,D), State(B
                                                  //| ,D)), List(State(A,D), State(C,D)), List(State(B,C), State(A,A)), List(Stat
                                                  //| e(B,C), State(D,D)), List(State(B,C), State(A,C)), List(State(B,C), State(B
                                                  //| ,C)), List(State(B,C), State(B,D)), List(State(B,C), State(B,A)), List(Stat
                                                  //| e(B,C), State(D,C)), List(State(B,D), State(A,A)), List(State(B,D), State(C
                                                  //| ,C)), List(State(B,D), State(A,D)), List(State(B,D), State(B,C)), List(Stat
                                                  //| e(B,D), State(B,D)), List(State(B,D), State(C,D)), List(State(B,D), State(B
                                                  //| ,A)), List(State(C,D), State(A,A)), List(State(C,D), State(B,B)), List(Stat
                                                  //| e(C,D), State(A,D)), List(State(C,D), State(B,D)), List(State(C,D), State(C
                                                  //| ,D)), List(State(C,D), State(C,A)), List(State(C,D), State(C,B)), List(Stat
                                                  //| e(B,A), State(C,C)), List(State(B,A), State(D,D)), List(State(B,A), State(B
                                                  //| ,C)), List(State(B,A), State(B,D)), List(State(B,A), State(B,A)), List(Stat
                                                  //| e(B,A), State(C,A)), List(State(B,A), State(D,A)), List(State(C,A), State(B
                                                  //| ,B)), List(State(C,A), State(D,D)), List(State(C,A), State(C,D)), List(Stat
                                                  //| e(C,A), State(B,A)), List(State(C,A), State(C,A)), List(State(C,A), State(D
                                                  //| ,A)), List(State(C,A), State(C,B)), List(State(D,A), State(B,B)), List(Stat
                                                  //| e(D,A), State(C,C)), List(State(D,A), State(B,A)), List(State(D,A), State(C
                                                  //| ,A)), List(State(D,A), State(D,A)), List(State(D,A), State(D,B)), List(Stat
                                                  //| e(D,A), State(D,C)), List(State(C,B), State(A,A)), List(State(C,B), State(D
                                                  //| ,D)), List(State(C,B), State(A,B)), List(State(C,B), State(C,D)), List(Stat
                                                  //| e(C,B), State(C,A)), List(State(C,B), State(C,B)), List(State(C,B), State(D
                                                  //| ,B)), List(State(D,B), State(A,A)), List(State(D,B), State(C,C)), List(Stat
                                                  //| e(D,B), State(A,B)), List(State(D,B), State(D,A)), List(State(D,B), State(C
                                                  //| ,B)), List(State(D,B), State(D,B)), List(State(D,B), State(D,C)), List(Stat
                                                  //| e(D,C), State(A,A)), List(State(D,C), State(B,B)), List(State(D,C), State(A
                                                  //| ,C)), List(State(D,C), State(B,C)), List(State(D,C), State(D,A)), List(Stat
                                                  //| e(D,C), State(D,B)), List(State(D,C), State(D,C)))
  }
  allnEquals2Routines.size                        //> res7: Int = 120
  allnEquals2Routines.foreach(println)            //> List(State(A,A), State(B,B))
                                                  //| List(State(A,A), State(C,C))
                                                  //| List(State(A,A), State(D,D))
                                                  //| List(State(A,A), State(B,C))
                                                  //| List(State(A,A), State(B,D))
                                                  //| List(State(A,A), State(C,D))
                                                  //| List(State(A,A), State(C,B))
                                                  //| List(State(A,A), State(D,B))
                                                  //| List(State(A,A), State(D,C))
                                                  //| List(State(B,B), State(A,A))
                                                  //| List(State(B,B), State(C,C))
                                                  //| List(State(B,B), State(D,D))
                                                  //| List(State(B,B), State(A,C))
                                                  //| List(State(B,B), State(A,D))
                                                  //| List(State(B,B), State(C,D))
                                                  //| List(State(B,B), State(C,A))
                                                  //| List(State(B,B), State(D,A))
                                                  //| List(State(B,B), State(D,C))
                                                  //| List(State(C,C), State(A,A))
                                                  //| List(State(C,C), State(B,B))
                                                  //| List(State(C,C), State(D,D))
                                                  //| List(State(C,C), State(A,B))
                                                  //| List(State(C,C), State(A,D))
                                                  //| List(State(C,C), State(B,D))
                                                  //| List(State(C,C), State(B,A))
                                                  //| List(State(C,C), State(D,A))
                                                  //| List(State(C,C), State(D,B))
                                                  //| List(State(D,D), State(A,A))
                                                  //| List(State(D,D), State(B,B))
                                                  //| List(State(D,D), State(C,C))
                                                  //| List(State(D,D), State(A,B))
                                                  //| List(State(D,D), State(A,C))
                                                  //| List(State(D,D), State(B,C))
                                                  //| List(State(D,D), State(B,A))
                                                  //| List(State(D,D), State(C,A))
                                                  //| List(State(D,D), State(C,B))
                                                  //| List(State(A,B), State(C,C))
                                                  //| List(State(A,B), State(D,D))
                                                  //| List(State(A,B), State(A,B))
                                                  //| List(State(A,B), State(A,C))
                                                  //| List(State(A,B), State(A,D))
                                                  //| List(State(A,B), State(C,B))
                                                  //| List(State(A,B), State(D,B))
                                                  //| List(State(A,C), State(B,B))
                                                  //| List(State(A,C), State(D,D))
                                                  //| List(State(A,C), State(A,B))
                                                  //| List(State(A,C), State(A,C))
                                                  //| List(State(A,C), State(A,D))
                                                  //| List(State(A,C), State(B,C))
                                                  //| List(State(A,C), State(D,C))
                                                  //| List(State(A,D), State(B,B))
                                                  //| List(State(A,D), State(C,C))
                                                  //| List(State(A,D), State(A,B))
                                                  //| List(State(A,D), State(A,C))
                                                  //| List(State(A,D), State(A,D))
                                                  //| List(State(A,D), State(B,D))
                                                  //| List(State(A,D), State(C,D))
                                                  //| List(State(B,C), State(A,A))
                                                  //| List(State(B,C), State(D,D))
                                                  //| List(State(B,C), State(A,C))
                                                  //| List(State(B,C), State(B,C))
                                                  //| List(State(B,C), State(B,D))
                                                  //| List(State(B,C), State(B,A))
                                                  //| List(State(B,C), State(D,C))
                                                  //| List(State(B,D), State(A,A))
                                                  //| List(State(B,D), State(C,C))
                                                  //| List(State(B,D), State(A,D))
                                                  //| List(State(B,D), State(B,C))
                                                  //| List(State(B,D), State(B,D))
                                                  //| List(State(B,D), State(C,D))
                                                  //| List(State(B,D), State(B,A))
                                                  //| List(State(C,D), State(A,A))
                                                  //| List(State(C,D), State(B,B))
                                                  //| List(State(C,D), State(A,D))
                                                  //| List(State(C,D), State(B,D))
                                                  //| List(State(C,D), State(C,D))
                                                  //| List(State(C,D), State(C,A))
                                                  //| List(State(C,D), State(C,B))
                                                  //| List(State(B,A), State(C,C))
                                                  //| List(State(B,A), State(D,D))
                                                  //| List(State(B,A), State(B,C))
                                                  //| List(State(B,A), State(B,D))
                                                  //| List(State(B,A), State(B,A))
                                                  //| List(State(B,A), State(C,A))
                                                  //| List(State(B,A), State(D,A))
                                                  //| List(State(C,A), State(B,B))
                                                  //| List(State(C,A), State(D,D))
                                                  //| List(State(C,A), State(C,D))
                                                  //| List(State(C,A), State(B,A))
                                                  //| List(State(C,A), State(C,A))
                                                  //| List(State(C,A), State(D,A))
                                                  //| List(State(C,A), State(C,B))
                                                  //| List(State(D,A), State(B,B))
                                                  //| List(State(D,A), State(C,C))
                                                  //| List(State(D,A), State(B,A))
                                                  //| List(State(D,A), State(C,A))
                                                  //| List(State(D,A), State(D,A))
                                                  //| List(State(D,A), State(D,B))
                                                  //| List(State(D,A), State(D,C))
                                                  //| List(State(C,B), State(A,A))
                                                  //| List(State(C,B), State(D,D))
                                                  //| List(State(C,B), State(A,B))
                                                  //| List(State(C,B), State(C,D))
                                                  //| List(State(C,B), State(C,A))
                                                  //| List(State(C,B), State(C,B))
                                                  //| List(State(C,B), State(D,B))
                                                  //| List(State(D,B), State(A,A))
                                                  //| List(State(D,B), State(C,C))
                                                  //| List(State(D,B), State(A,B))
                                                  //| List(State(D,B), State(D,A))
                                                  //| List(State(D,B), State(C,B))
                                                  //| List(State(D,B), State(D,B))
                                                  //| List(State(D,B), State(D,C))
                                                  //| List(State(D,C), State(A,A))
                                                  //| List(State(D,C), State(B,B))
                                                  //| List(State(D,C), State(A,C))
                                                  //| List(State(D,C), State(B,C))
                                                  //| List(State(D,C), State(D,A))
                                                  //| List(State(D,C), State(D,B))
                                                  //| List(State(D,C), State(D,C))

  val cacheOfRoutines = scala.collection.mutable.Map[Int, List[Routine]]()
                                                  //> cacheOfRoutines  : scala.collection.mutable.Map[Int,List[List[ponder.State]
                                                  //| ]] = Map()

  def generateRoutinesOfLengthN(n: Int): List[Routine] = n match {
    case 1 => generate16States.map(s => List(s))
    case _ => {
      for {
        statesSoFar <- cacheOfRoutines.getOrElseUpdate(n - 1, generateRoutinesOfLengthN(n - 1))
        stateToAppend <- generate16States
        if (doesntViolateAnyRules(statesSoFar, stateToAppend))
        /*  val firstState = statesSoFar.last
    if (
     doesntViolateRule1(statesSoFar.drop( statesSoFar.size - 3) :+ stateToAppend) &&
	   doesntViolateRule2(firstState, stateToAppend) &&
	   doesntViolateRule3(firstState, stateToAppend) &&
	   doesntViolateRule4(firstState, stateToAppend)
	  )*/
      } yield statesSoFar :+ stateToAppend
    }
  }                                               //> generateRoutinesOfLengthN: (n: Int)List[ponder.Routine]

  def printRoutines(routines: List[Routine]) =
    {
      routines.zipWithIndex.foreach {
        case (routine, i) =>
          println("Routine " + (i + 1))
          println(routine.map(s => s.left).mkString)
          println(routine.map(s => s.right).mkString)
      }
    }                                             //> printRoutines: (routines: List[ponder.Routine])Unit

  val routines1 = generateRoutinesOfLengthN(1)    //> routines1  : List[ponder.Routine] = List(List(State(A,A)), List(State(B,B))
                                                  //| , List(State(C,C)), List(State(D,D)), List(State(A,B)), List(State(A,C)), L
                                                  //| ist(State(A,D)), List(State(B,C)), List(State(B,D)), List(State(C,D)), List
                                                  //| (State(B,A)), List(State(C,A)), List(State(D,A)), List(State(C,B)), List(St
                                                  //| ate(D,B)), List(State(D,C)))
  printRoutines(routines1)                        //> Routine 1
                                                  //| A
                                                  //| A
                                                  //| Routine 2
                                                  //| B
                                                  //| B
                                                  //| Routine 3
                                                  //| C
                                                  //| C
                                                  //| Routine 4
                                                  //| D
                                                  //| D
                                                  //| Routine 5
                                                  //| A
                                                  //| B
                                                  //| Routine 6
                                                  //| A
                                                  //| C
                                                  //| Routine 7
                                                  //| A
                                                  //| D
                                                  //| Routine 8
                                                  //| B
                                                  //| C
                                                  //| Routine 9
                                                  //| B
                                                  //| D
                                                  //| Routine 10
                                                  //| C
                                                  //| D
                                                  //| Routine 11
                                                  //| B
                                                  //| A
                                                  //| Routine 12
                                                  //| C
                                                  //| A
                                                  //| Routine 13
                                                  //| D
                                                  //| A
                                                  //| Routine 14
                                                  //| C
                                                  //| B
                                                  //| Routine 15
                                                  //| D
                                                  //| B
                                                  //| Routine 16
                                                  //| D
                                                  //| C
  generateRoutinesOfLengthN(1).size               //> res8: Int = 16
  generateRoutinesOfLengthN(1).foreach(println)   //> List(State(A,A))
                                                  //| List(State(B,B))
                                                  //| List(State(C,C))
                                                  //| List(State(D,D))
                                                  //| List(State(A,B))
                                                  //| List(State(A,C))
                                                  //| List(State(A,D))
                                                  //| List(State(B,C))
                                                  //| List(State(B,D))
                                                  //| List(State(C,D))
                                                  //| List(State(B,A))
                                                  //| List(State(C,A))
                                                  //| List(State(D,A))
                                                  //| List(State(C,B))
                                                  //| List(State(D,B))
                                                  //| List(State(D,C))
  generateRoutinesOfLengthN(2).size               //> res9: Int = 120
  //generateRoutinesOfLengthN(2).foreach(println)
  //printRoutines(generateRoutinesOfLengthN(2))

  // scalac ponder.sc && time scala -J-Xms11g -J-Xmx11g ponder
  // real    1m47.293s

  //def main(in: Array[String]): Unit = {
   /* (3 to 5).map(i =>
      println("" + i + " " + generateRoutinesOfLengthN(i).size))
 3 912
 4 6300
 5 45876
 */
    /*
3 912
4 6300
5 45876
6 331884
7 2401092
8 17342172
 */

    /*
 So I think we can skip to the transition matrix stuff now. All we need to consider if a routine can append a new state is the last 3 states.
 We can make a map of the 912 3-state "meta-states" to the 16 possible appendable states.
 This can be represented in a 912x16 transition matrix.
 ... OR! In adjacency list format. We make an index to each of the 912 states, and make a Map[ Int, List[Int] ] of the indices that will correspond to the newly
 create 3-state.
 For example say one of our 3-states is index 1, ->
 AAA
 AAA
 We know that there are at most 16 4-states buildable from that. For each 4-state, we drop the first state to get a new 3-state. Since all the 3-states are indexed, we
 can refer to them by their indices.
 */
    val three_States = generateRoutinesOfLengthN(3)
                                                  //> three_States  : List[ponder.Routine] = List(List(State(A,A), State(B,B), St
                                                  //| ate(A,A)), List(State(A,A), State(B,B), State(C,C)), List(State(A,A), State
                                                  //| (B,B), State(D,D)), List(State(A,A), State(B,B), State(A,C)), List(State(A,
                                                  //| A), State(B,B), State(A,D)), List(State(A,A), State(B,B), State(C,D)), List
                                                  //| (State(A,A), State(B,B), State(C,A)), List(State(A,A), State(B,B), State(D,
                                                  //| A)), List(State(A,A), State(B,B), State(D,C)), List(State(A,A), State(C,C),
                                                  //|  State(A,A)), List(State(A,A), State(C,C), State(B,B)), List(State(A,A), St
                                                  //| ate(C,C), State(D,D)), List(State(A,A), State(C,C), State(A,B)), List(State
                                                  //| (A,A), State(C,C), State(A,D)), List(State(A,A), State(C,C), State(B,D)), L
                                                  //| ist(State(A,A), State(C,C), State(B,A)), List(State(A,A), State(C,C), State
                                                  //| (D,A)), List(State(A,A), State(C,C), State(D,B)), List(State(A,A), State(D,
                                                  //| D), State(A,A)), List(State(A,A), State(D,D), State(B,B)), List(State(A,A),
                                                  //|  State(D,D), State(C,C)), List(State(A,A), State(D,D), State(A,B)), List(St
                                                  //| ate(A,A), State(D,D), State(A,C)), List(State(A,A), State(D,D), State(B,C))
                                                  //| , List(State(A,A), State(D,D), State(B,A)), List(State(A,A), State(D,D), St
                                                  //| ate(C,A)), List(State(A,A), State(D,D), State(C,B)), List(State(A,A), State
                                                  //| (B,C), State(A,A)), List(State(A,A), State(B,C), State(D,D)), List(State(A,
                                                  //| A), State(B,C), State(A,C)), List(State(A,A), State(B,C), State(B,C)), List
                                                  //| (State(A,A), State(B,C), State(B,D)), List(State(A,A), State(B,C), State(B,
                                                  //| A)), List(State(A,A), State(B,C), State(D,C)), List(State(A,A), State(B,D),
                                                  //|  State(A,A)), List(State(A,A), State(B,D), State(C,C)), List(State(A,A), St
                                                  //| ate(B,D), State(A,D)), List(State(A,A), State(B,D), State(B,C)), List(State
                                                  //| (A,A), State(B,D), State(B,D)), List(State(A,A), State(B,D), State(C,D)), L
                                                  //| ist(State(A,A), State(B,D), State(B,A)), List(State(A,A), State(C,D), State
                                                  //| (A,A)), List(State(A,A), State(C,D), State(B,B)), List(State(A,A), State(C,
                                                  //| D), State(A,D)), List(State(A,A), State(C,D), State(B,D)), List(State(A,A),
                                                  //|  State(C,D), State(C,D)), List(State(A,A), State(C,D), State(C,A)), List(St
                                                  //| ate(A,A), State(C,D), State(C,B)), List(State(A,A), State(C,B), State(A,A))
                                                  //| , List(State(A,A), State(C,B), State(D,D)), List(State(A,A), State(C,B), St
                                                  //| ate(A,B)), List(State(A,A), State(C,B), State(C,D)), List(State(A,A), State
                                                  //| (C,B), State(C,A)), List(State(A,A), State(C,B), State(C,B)), List(State(A,
                                                  //| A), State(C,B), State(D,B)), List(State(A,A), State(D,B), State(A,A)), List
                                                  //| (State(A,A), State(D,B), State(C,C)), List(State(A,A), State(D,B), State(A,
                                                  //| B)), List(State(A,A), State(D,B), State(D,A)), List(State(A,A), State(D,B),
                                                  //|  State(C,B)), List(State(A,A), State(D,B), State(D,B)), List(State(A,A), St
                                                  //| ate(D,B), State(D,C)), List(State(A,A), State(D,C), State(A,A)), List(State
                                                  //| (A,A), State(D,C), State(B,B)), List(State(A,A), State(D,C), State(A,C)), L
                                                  //| ist(State(A,A), State(D,C), State(B,C)), List(State(A,A), State(D,C), State
                                                  //| (D,A)), List(State(A,A), State(D,C), State(D,B)), List(State(A,A), State(D,
                                                  //| C), State(D,C)), List(State(B,B), State(A,A), State(B,B)), List(State(B,B),
                                                  //|  State(A,A), State(C,C)), List(State(B,B), State(A,A), State(D,D)), List(St
                                                  //| ate(B,B), State(A,A), State(B,C)), List(State(B,B), State(A,A), State(B,D))
                                                  //| , List(State(B,B), State(A,A), State(C,D)), List(State(B,B), State(A,A), St
                                                  //| ate(C,B)), List(State(B,B), State(A,A), State(D,B)), List(State(B,B), State
                                                  //| (A,A), State(D,C)), List(State(B,B), State(C,C), State(A,A)), List(State(B,
                                                  //| B), State(C,C), State(B,B)), List(State(B,B), State(C,C), State(D,D)), List
                                                  //| (State(B,B), State(C,C), State(A,B)), List(State(B,B), State(C,C), State(A,
                                                  //| D)), List(State(B,B), State(C,C), State(B,D)), List(State(B,B), State(C,C),
                                                  //|  State(B,A)), List(State(B,B), State(C,C), State(D,A)), List(State(B,B), St
                                                  //| ate(C,C), State(D,B)), List(State(B,B), State(D,D), State(A,A)), List(State
                                                  //| (B,B), State(D,D), State(B,B)), List(State(B,B), State(D,D), State(C,C)), L
                                                  //| ist(State(B,B), State(D,D), State(A,B)), List(State(B,B), State(D,D), State
                                                  //| (A,C)), List(State(B,B), State(D,D), State(B,C)), List(State(B,B), State(D,
                                                  //| D), State(B,A)), List(State(B,B), State(D,D), State(C,A)), List(State(B,B),
                                                  //|  State(D,D), State(C,B)), List(State(B,B), State(A,C), State(B,B)), List(St
                                                  //| ate(B,B), State(A,C), State(D,D)), List(State(B,B), State(A,C), State(A,B))
                                                  //| , List(State(B,B), State(A,C), State(A,C)), List(State(B,B), State(A,C), St
                                                  //| ate(A,D)), List(State(B,B), State(A,C), State(B,C)), List(State(B,B), State
                                                  //| (A,C), State(D,C)), List(State(B,B), State(A,D), State(B,B)), List(State(B,
                                                  //| B), State(A,D), State(C,C)), List(State(B,B), State(A,D), State(A,B)), List
                                                  //| (State(B,B), State(A,D), State(A,C)), List(State(B,B), State(A,D), State(A,
                                                  //| D)), List(State(B,B), State(A,D), State(B,D)), List(State(B,B), State(A,D),
                                                  //|  State(C,D)), List(State(B,B), State(C,D), State(A,A)), List(State(B,B), St
                                                  //| ate(C,D), State(B,B)), List(State(B,B), State(C,D), State(A,D)), List(State
                                                  //| (B,B), State(C,D), State(B,D)), List(State(B,B), State(C,D), State(C,D)), L
                                                  //| ist(State(B,B), State(C,D), State(C,A)), List(State(B,B), State(C,D), State
                                                  //| (C,B)), List(State(B,B), State(C,A), State(B,B)), List(State(B,B), State(C,
                                                  //| A), State(D,D)), List(State(B,B), State(C,A), State(C,D)), List(State(B,B),
                                                  //|  State(C,A), State(B,A)), List(State(B,B), State(C,A), State(C,A)), List(St
                                                  //| ate(B,B), State(C,A), State(D,A)), List(State(B,B), State(C,A), State(C,B))
                                                  //| , List(State(B,B), State(D,A), State(B,B)), List(State(B,B), State(D,A), St
                                                  //| ate(C,C)), List(State(B,B), State(D,A), State(B,A)), List(State(B,B), State
                                                  //| (D,A), State(C,A)), List(State(B,B), State(D,A), State(D,A)), List(State(B,
                                                  //| B), State(D,A), State(D,B)), List(State(B,B), State(D,A), State(D,C)), List
                                                  //| (State(B,B), State(D,C), State(A,A)), List(State(B,B), State(D,C), State(B,
                                                  //| B)), List(State(B,B), State(D,C), State(A,C)), List(State(B,B), State(D,C),
                                                  //|  State(B,C)), List(State(B,B), State(D,C), State(D,A)), List(State(B,B), St
                                                  //| ate(D,C), State(D,B)), List(State(B,B), State(D,C), State(D,C)), List(State
                                                  //| (C,C), State(A,A), State(B,B)), List(State(C,C), State(A,A), State(C,C)), L
                                                  //| ist(State(C,C), State(A,A), State(D,D)), List(State(C,C), State(A,A), State
                                                  //| (B,C)), List(State(C,C), State(A,A), State(B,D)), List(State(C,C), State(A,
                                                  //| A), State(C,D)), List(State(C,C), State(A,A), State(C,B)), List(State(C,C),
                                                  //|  State(A,A), State(D,B)), List(State(C,C), State(A,A), State(D,C)), List(St
                                                  //| ate(C,C), State(B,B), State(A,A)), List(State(C,C), State(B,B), State(C,C))
                                                  //| , List(State(C,C), State(B,B), State(D,D)), List(State(C,C), State(B,B), St
                                                  //| ate(A,C)), List(State(C,C), State(B,B), State(A,D)), List(State(C,C), State
                                                  //| (B,B), State(C,D)), List(State(C,C), State(B,B), State(C,A)), List(State(C,
                                                  //| C), State(B,B), State(D,A)), List(State(C,C), State(B,B), State(D,C)), List
                                                  //| (State(C,C), State(D,D), State(A,A)), List(State(C,C), State(D,D), State(B,
                                                  //| B)), List(State(C,C), State(D,D), State(C,C)), List(State(C,C), State(D,D),
                                                  //|  State(A,B)), List(State(C,C), State(D,D), State(A,C)), List(State(C,C), St
                                                  //| ate(D,D), State(B,C)), List(State(C,C), State(D,D), State(B,A)), List(State
                                                  //| (C,C), State(D,D), State(C,A)), List(State(C,C), State(D,D), State(C,B)), L
                                                  //| ist(State(C,C), State(A,B), State(C,C)), List(State(C,C), State(A,B), State
                                                  //| (D,D)), List(State(C,C), State(A,B), State(A,B)), List(State(C,C), State(A,
                                                  //| B), State(A,C)), List(State(C,C), State(A,B), State(A,D)), List(State(C,C),
                                                  //|  State(A,B), State(C,B)), List(State(C,C), State(A,B), State(D,B)), List(St
                                                  //| ate(C,C), State(A,D), State(B,B)), List(State(C,C), State(A,D), State(C,C))
                                                  //| , List(State(C,C), State(A,D), State(A,B)), List(State(C,C), State(A,D), St
                                                  //| ate(A,C)), List(State(C,C), State(A,D), State(A,D)), List(State(C,C), State
                                                  //| (A,D), State(B,D)), List(State(C,C), State(A,D), State(C,D)), List(State(C,
                                                  //| C), State(B,D), State(A,A)), List(State(C,C), State(B,D), State(C,C)), List
                                                  //| (State(C,C), State(B,D), State(A,D)), List(State(C,C), State(B,D), State(B,
                                                  //| C)), List(State(C,C), State(B,D), State(B,D)), List(State(C,C), State(B,D),
                                                  //|  State(C,D)), List(State(C,C), State(B,D), State(B,A)), List(State(C,C), St
                                                  //| ate(B,A), State(C,C)), List(State(C,C), State(B,A), State(D,D)), List(State
                                                  //| (C,C), State(B,A), State(B,C)), List(State(C,C), State(B,A), State(B,D)), L
                                                  //| ist(State(C,C), State(B,A), State(B,A)), List(State(C,C), State(B,A), State
                                                  //| (C,A)), List(State(C,C), State(B,A), State(D,A)), List(State(C,C), State(D,
                                                  //| A), State(B,B)), List(State(C,C), State(D,A), State(C,C)), List(State(C,C),
                                                  //|  State(D,A), State(B,A)), List(State(C,C), State(D,A), State(C,A)), List(St
                                                  //| ate(C,C), State(D,A), State(D,A)), List(State(C,C), State(D,A), State(D,B))
                                                  //| , List(State(C,C), State(D,A), State(D,C)), List(State(C,C), State(D,B), St
                                                  //| ate(A,A)), List(State(C,C), State(D,B), State(C,C)), List(State(C,C), State
                                                  //| (D,B), State(A,B)), List(State(C,C), State(D,B), State(D,A)), List(State(C,
                                                  //| C), State(D,B), State(C,B)), List(State(C,C), State(D,B), State(D,B)), List
                                                  //| (State(C,C), State(D,B), State(D,C)), List(State(D,D), State(A,A), State(B,
                                                  //| B)), List(State(D,D), State(A,A), State(C,C)), List(State(D,D), State(A,A),
                                                  //|  State(D,D)), List(State(D,D), State(A,A), State(B,C)), List(State(D,D), St
                                                  //| ate(A,A), State(B,D)), List(State(D,D), State(A,A), State(C,D)), List(State
                                                  //| (D,D), State(A,A), State(C,B)), List(State(D,D), State(A,A), State(D,B)), L
                                                  //| ist(State(D,D), State(A,A), State(D,C)), List(State(D,D), State(B,B), State
                                                  //| (A,A)), List(State(D,D), State(B,B), State(C,C)), List(State(D,D), State(B,
                                                  //| B), State(D,D)), List(State(D,D), State(B,B), State(A,C)), List(State(D,D),
                                                  //|  State(B,B), State(A,D)), List(State(D,D), State(B,B), State(C,D)), List(St
                                                  //| ate(D,D), State(B,B), State(C,A)), List(State(D,D), State(B,B), State(D,A))
                                                  //| , List(State(D,D), State(B,B), State(D,C)), List(State(D,D), State(C,C), St
                                                  //| ate(A,A)), List(State(D,D), State(C,C), State(B,B)), List(State(D,D), State
                                                  //| (C,C), State(D,D)), List(State(D,D), State(C,C), State(A,B)), List(State(D,
                                                  //| D), State(C,C), State(A,D)), List(State(D,D), State(C,C), State(B,D)), List
                                                  //| (State(D,D), State(C,C), State(B,A)), List(State(D,D), State(C,C), State(D,
                                                  //| A)), List(State(D,D), State(C,C), State(D,B)), List(State(D,D), State(A,B),
                                                  //|  State(C,C)), List(State(D,D), State(A,B), State(D,D)), List(State(D,D), St
                                                  //| ate(A,B), State(A,B)), List(State(D,D), State(A,B), State(A,C)), List(State
                                                  //| (D,D), State(A,B), State(A,D)), List(State(D,D), State(A,B), State(C,B)), L
                                                  //| ist(State(D,D), State(A,B), State(D,B)), List(State(D,D), State(A,C), State
                                                  //| (B,B)), List(State(D,D), State(A,C), State(D,D)), List(State(D,D), State(A,
                                                  //| C), State(A,B)), List(State(D,D), State(A,C), State(A,C)), List(State(D,D),
                                                  //|  State(A,C), State(A,D)), List(State(D,D), State(A,C), State(B,C)), List(St
                                                  //| ate(D,D), State(A,C), State(D,C)), List(State(D,D), State(B,C), State(A,A))
                                                  //| , List(State(D,D), State(B,C), State(D,D)), List(State(D,D), State(B,C), St
                                                  //| ate(A,C)), List(State(D,D), State(B,C), State(B,C)), List(State(D,D), State
                                                  //| (B,C), State(B,D)), List(State(D,D), State(B,C), State(B,A)), List(State(D,
                                                  //| D), State(B,C), State(D,C)), List(State(D,D), State(B,A), State(C,C)), List
                                                  //| (State(D,D), State(B,A), State(D,D)), List(State(D,D), State(B,A), State(B,
                                                  //| C)), List(State(D,D), State(B,A), State(B,D)), List(State(D,D), State(B,A),
                                                  //|  State(B,A)), List(State(D,D), State(B,A), State(C,A)), List(State(D,D), St
                                                  //| ate(B,A), State(D,A)), List(State(D,D), State(C,A), State(B,B)), List(State
                                                  //| (D,D), State(C,A), State(D,D)), List(State(D,D), State(C,A), State(C,D)), L
                                                  //| ist(State(D,D), State(C,A), State(B,A)), List(State(D,D), State(C,A), State
                                                  //| (C,A)), List(State(D,D), State(C,A), State(D,A)), List(State(D,D), State(C,
                                                  //| A), State(C,B)), List(State(D,D), State(C,B), State(A,A)), List(State(D,D),
                                                  //|  State(C,B), State(D,D)), List(State(D,D), State(C,B), State(A,B)), List(St
                                                  //| ate(D,D), State(C,B), State(C,D)), List(State(D,D), State(C,B), State(C,A))
                                                  //| , List(State(D,D), State(C,B), State(C,B)), List(State(D,D), State(C,B), St
                                                  //| ate(D,B)), List(State(A,B), State(C,C), State(A,A)), List(State(A,B), State
                                                  //| (C,C), State(B,B)), List(State(A,B), State(C,C), State(D,D)), List(State(A,
                                                  //| B), State(C,C), State(A,B)), List(State(A,B), State(C,C), State(A,D)), List
                                                  //| (State(A,B), State(C,C), State(B,D)), List(State(A,B), State(C,C), State(B,
                                                  //| A)), List(State(A,B), State(C,C), State(D,A)), List(State(A,B), State(C,C),
                                                  //|  State(D,B)), List(State(A,B), State(D,D), State(A,A)), List(State(A,B), St
                                                  //| ate(D,D), State(B,B)), List(State(A,B), State(D,D), State(C,C)), List(State
                                                  //| (A,B), State(D,D), State(A,B)), List(State(A,B), State(D,D), State(A,C)), L
                                                  //| ist(State(A,B), State(D,D), State(B,C)), List(State(A,B), State(D,D), State
                                                  //| (B,A)), List(State(A,B), State(D,D), State(C,A)), List(State(A,B), State(D,
                                                  //| D), State(C,B)), List(State(A,B), State(A,B), State(C,C)), List(State(A,B),
                                                  //|  State(A,B), State(D,D)), List(State(A,B), State(A,B), State(A,B)), List(St
                                                  //| ate(A,B), State(A,B), State(A,C)), List(State(A,B), State(A,B), State(A,D))
                                                  //| , List(State(A,B), State(A,B), State(C,B)), List(State(A,B), State(A,B), St
                                                  //| ate(D,B)), List(State(A,B), State(A,C), State(B,B)), List(State(A,B), State
                                                  //| (A,C), State(D,D)), List(State(A,B), State(A,C), State(A,B)), List(State(A,
                                                  //| B), State(A,C), State(A,C)), List(State(A,B), State(A,C), State(A,D)), List
                                                  //| (State(A,B), State(A,C), State(B,C)), List(State(A,B), State(A,C), State(D,
                                                  //| C)), List(State(A,B), State(A,D), State(B,B)), List(State(A,B), State(A,D),
                                                  //|  State(C,C)), List(State(A,B), State(A,D), State(A,B)), List(State(A,B), St
                                                  //| ate(A,D), State(A,C)), List(State(A,B), State(A,D), State(A,D)), List(State
                                                  //| (A,B), State(A,D), State(B,D)), List(State(A,B), State(A,D), State(C,D)), L
                                                  //| ist(State(A,B), State(C,B), State(A,A)), List(State(A,B), State(C,B), State
                                                  //| (D,D)), List(State(A,B), State(C,B), State(A,B)), List(State(A,B), State(C,
                                                  //| B), State(C,D)), List(State(A,B), State(C,B), State(C,A)), List(State(A,B),
                                                  //|  State(C,B), State(C,B)), List(State(A,B), State(C,B), State(D,B)), List(St
                                                  //| ate(A,B), State(D,B), State(A,A)), List(State(A,B), State(D,B), State(C,C))
                                                  //| , List(State(A,B), State(D,B), State(A,B)), List(State(A,B), State(D,B), St
                                                  //| ate(D,A)), List(State(A,B), State(D,B), State(C,B)), List(State(A,B), State
                                                  //| (D,B), State(D,B)), List(State(A,B), State(D,B), State(D,C)), List(State(A,
                                                  //| C), State(B,B), State(A,A)), List(State(A,C), State(B,B), State(C,C)), List
                                                  //| (State(A,C), State(B,B), State(D,D)), List(State(A,C), State(B,B), State(A,
                                                  //| C)), List(State(A,C), State(B,B), State(A,D)), List(State(A,C), State(B,B),
                                                  //|  State(C,D)), List(State(A,C), State(B,B), State(C,A)), List(State(A,C), St
                                                  //| ate(B,B), State(D,A)), List(State(A,C), State(B,B), State(D,C)), List(State
                                                  //| (A,C), State(D,D), State(A,A)), List(State(A,C), State(D,D), State(B,B)), L
                                                  //| ist(State(A,C), State(D,D), State(C,C)), List(State(A,C), State(D,D), State
                                                  //| (A,B)), List(State(A,C), State(D,D), State(A,C)), List(State(A,C), State(D,
                                                  //| D), State(B,C)), List(State(A,C), State(D,D), State(B,A)), List(State(A,C),
                                                  //|  State(D,D), State(C,A)), List(State(A,C), State(D,D), State(C,B)), List(St
                                                  //| ate(A,C), State(A,B), State(C,C)), List(State(A,C), State(A,B), State(D,D))
                                                  //| , List(State(A,C), State(A,B), State(A,B)), List(State(A,C), State(A,B), St
                                                  //| ate(A,C)), List(State(A,C), State(A,B), State(A,D)), List(State(A,C), State
                                                  //| (A,B), State(C,B)), List(State(A,C), State(A,B), State(D,B)), List(State(A,
                                                  //| C), State(A,C), State(B,B)), List(State(A,C), State(A,C), State(D,D)), List
                                                  //| (State(A,C), State(A,C), State(A,B)), List(State(A,C), State(A,C), State(A,
                                                  //| C)), List(State(A,C), State(A,C), State(A,D)), List(State(A,C), State(A,C),
                                                  //|  State(B,C)), List(State(A,C), State(A,C), State(D,C)), List(State(A,C), St
                                                  //| ate(A,D), State(B,B)), List(State(A,C), State(A,D), State(C,C)), List(State
                                                  //| (A,C), State(A,D), State(A,B)), List(State(A,C), State(A,D), State(A,C)), L
                                                  //| ist(State(A,C), State(A,D), State(A,D)), List(State(A,C), State(A,D), State
                                                  //| (B,D)), List(State(A,C), State(A,D), State(C,D)), List(State(A,C), State(B,
                                                  //| C), State(A,A)), List(State(A,C), State(B,C), State(D,D)), List(State(A,C),
                                                  //|  State(B,C), State(A,C)), List(State(A,C), State(B,C), State(B,C)), List(St
                                                  //| ate(A,C), State(B,C), State(B,D)), List(State(A,C), State(B,C), State(B,A))
                                                  //| , List(State(A,C), State(B,C), State(D,C)), List(State(A,C), State(D,C), St
                                                  //| ate(A,A)), List(State(A,C), State(D,C), State(B,B)), List(State(A,C), State
                                                  //| (D,C), State(A,C)), List(State(A,C), State(D,C), State(B,C)), List(State(A,
                                                  //| C), State(D,C), State(D,A)), List(State(A,C), State(D,C), State(D,B)), List
                                                  //| (State(A,C), State(D,C), State(D,C)), List(State(A,D), State(B,B), State(A,
                                                  //| A)), List(State(A,D), State(B,B), State(C,C)), List(State(A,D), State(B,B),
                                                  //|  State(D,D)), List(State(A,D), State(B,B), State(A,C)), List(State(A,D), St
                                                  //| ate(B,B), State(A,D)), List(State(A,D), State(B,B), State(C,D)), List(State
                                                  //| (A,D), State(B,B), State(C,A)), List(State(A,D), State(B,B), State(D,A)), L
                                                  //| ist(State(A,D), State(B,B), State(D,C)), List(State(A,D), State(C,C), State
                                                  //| (A,A)), List(State(A,D), State(C,C), State(B,B)), List(State(A,D), State(C,
                                                  //| C), State(D,D)), List(State(A,D), State(C,C), State(A,B)), List(State(A,D),
                                                  //|  State(C,C), State(A,D)), List(State(A,D), State(C,C), State(B,D)), List(St
                                                  //| ate(A,D), State(C,C), State(B,A)), List(State(A,D), State(C,C), State(D,A))
                                                  //| , List(State(A,D), State(C,C), State(D,B)), List(State(A,D), State(A,B), St
                                                  //| ate(C,C)), List(State(A,D), State(A,B), State(D,D)), List(State(A,D), State
                                                  //| (A,B), State(A,B)), List(State(A,D), State(A,B), State(A,C)), List(State(A,
                                                  //| D), State(A,B), State(A,D)), List(State(A,D), State(A,B), State(C,B)), List
                                                  //| (State(A,D), State(A,B), State(D,B)), List(State(A,D), State(A,C), State(B,
                                                  //| B)), List(State(A,D), State(A,C), State(D,D)), List(State(A,D), State(A,C),
                                                  //|  State(A,B)), List(State(A,D), State(A,C), State(A,C)), List(State(A,D), St
                                                  //| ate(A,C), State(A,D)), List(State(A,D), State(A,C), State(B,C)), List(State
                                                  //| (A,D), State(A,C), State(D,C)), List(State(A,D), State(A,D), State(B,B)), L
                                                  //| ist(State(A,D), State(A,D), State(C,C)), List(State(A,D), State(A,D), State
                                                  //| (A,B)), List(State(A,D), State(A,D), State(A,C)), List(State(A,D), State(A,
                                                  //| D), State(A,D)), List(State(A,D), State(A,D), State(B,D)), List(State(A,D),
                                                  //|  State(A,D), State(C,D)), List(State(A,D), State(B,D), State(A,A)), List(St
                                                  //| ate(A,D), State(B,D), State(C,C)), List(State(A,D), State(B,D), State(A,D))
                                                  //| , List(State(A,D), State(B,D), State(B,C)), List(State(A,D), State(B,D), St
                                                  //| ate(B,D)), List(State(A,D), State(B,D), State(C,D)), List(State(A,D), State
                                                  //| (B,D), State(B,A)), List(State(A,D), State(C,D), State(A,A)), List(State(A,
                                                  //| D), State(C,D), State(B,B)), List(State(A,D), State(C,D), State(A,D)), List
                                                  //| (State(A,D), State(C,D), State(B,D)), List(State(A,D), State(C,D), State(C,
                                                  //| D)), List(State(A,D), State(C,D), State(C,A)), List(State(A,D), State(C,D),
                                                  //|  State(C,B)), List(State(B,C), State(A,A), State(B,B)), List(State(B,C), St
                                                  //| ate(A,A), State(C,C)), List(State(B,C), State(A,A), State(D,D)), List(State
                                                  //| (B,C), State(A,A), State(B,C)), List(State(B,C), State(A,A), State(B,D)), L
                                                  //| ist(State(B,C), State(A,A), State(C,D)), List(State(B,C), State(A,A), State
                                                  //| (C,B)), List(State(B,C), State(A,A), State(D,B)), List(State(B,C), State(A,
                                                  //| A), State(D,C)), List(State(B,C), State(D,D), State(A,A)), List(State(B,C),
                                                  //|  State(D,D), State(B,B)), List(State(B,C), State(D,D), State(C,C)), List(St
                                                  //| ate(B,C), State(D,D), State(A,B)), List(State(B,C), State(D,D), State(A,C))
                                                  //| , List(State(B,C), State(D,D), State(B,C)), List(State(B,C), State(D,D), St
                                                  //| ate(B,A)), List(State(B,C), State(D,D), State(C,A)), List(State(B,C), State
                                                  //| (D,D), State(C,B)), List(State(B,C), State(A,C), State(B,B)), List(State(B,
                                                  //| C), State(A,C), State(D,D)), List(State(B,C), State(A,C), State(A,B)), List
                                                  //| (State(B,C), State(A,C), State(A,C)), List(State(B,C), State(A,C), State(A,
                                                  //| D)), List(State(B,C), State(A,C), State(B,C)), List(State(B,C), State(A,C),
                                                  //|  State(D,C)), List(State(B,C), State(B,C), State(A,A)), List(State(B,C), St
                                                  //| ate(B,C), State(D,D)), List(State(B,C), State(B,C), State(A,C)), List(State
                                                  //| (B,C), State(B,C), State(B,C)), List(State(B,C), State(B,C), State(B,D)), L
                                                  //| ist(State(B,C), State(B,C), State(B,A)), List(State(B,C), State(B,C), State
                                                  //| (D,C)), List(State(B,C), State(B,D), State(A,A)), List(State(B,C), State(B,
                                                  //| D), State(C,C)), List(State(B,C), State(B,D), State(A,D)), List(State(B,C),
                                                  //|  State(B,D), State(B,C)), List(State(B,C), State(B,D), State(B,D)), List(St
                                                  //| ate(B,C), State(B,D), State(C,D)), List(State(B,C), State(B,D), State(B,A))
                                                  //| , List(State(B,C), State(B,A), State(C,C)), List(State(B,C), State(B,A), St
                                                  //| ate(D,D)), List(State(B,C), State(B,A), State(B,C)), List(State(B,C), State
                                                  //| (B,A), State(B,D)), List(State(B,C), State(B,A), State(B,A)), List(State(B,
                                                  //| C), State(B,A), State(C,A)), List(State(B,C), State(B,A), State(D,A)), List
                                                  //| (State(B,C), State(D,C), State(A,A)), List(State(B,C), State(D,C), State(B,
                                                  //| B)), List(State(B,C), State(D,C), State(A,C)), List(State(B,C), State(D,C),
                                                  //|  State(B,C)), List(State(B,C), State(D,C), State(D,A)), List(State(B,C), St
                                                  //| ate(D,C), State(D,B)), List(State(B,C), State(D,C), State(D,C)), List(State
                                                  //| (B,D), State(A,A), State(B,B)), List(State(B,D), State(A,A), State(C,C)), L
                                                  //| ist(State(B,D), State(A,A), State(D,D)), List(State(B,D), State(A,A), State
                                                  //| (B,C)), List(State(B,D), State(A,A), State(B,D)), List(State(B,D), State(A,
                                                  //| A), State(C,D)), List(State(B,D), State(A,A), State(C,B)), List(State(B,D),
                                                  //|  State(A,A), State(D,B)), List(State(B,D), State(A,A), State(D,C)), List(St
                                                  //| ate(B,D), State(C,C), State(A,A)), List(State(B,D), State(C,C), State(B,B))
                                                  //| , List(State(B,D), State(C,C), State(D,D)), List(State(B,D), State(C,C), St
                                                  //| ate(A,B)), List(State(B,D), State(C,C), State(A,D)), List(State(B,D), State
                                                  //| (C,C), State(B,D)), List(State(B,D), State(C,C), State(B,A)), List(State(B,
                                                  //| D), State(C,C), State(D,A)), List(State(B,D), State(C,C), State(D,B)), List
                                                  //| (State(B,D), State(A,D), State(B,B)), List(State(B,D), State(A,D), State(C,
                                                  //| C)), List(State(B,D), State(A,D), State(A,B)), List(State(B,D), State(A,D),
                                                  //|  State(A,C)), List(State(B,D), State(A,D), State(A,D)), List(State(B,D), St
                                                  //| ate(A,D), State(B,D)), List(State(B,D), State(A,D), State(C,D)), List(State
                                                  //| (B,D), State(B,C), State(A,A)), List(State(B,D), State(B,C), State(D,D)), L
                                                  //| ist(State(B,D), State(B,C), State(A,C)), List(State(B,D), State(B,C), State
                                                  //| (B,C)), List(State(B,D), State(B,C), State(B,D)), List(State(B,D), State(B,
                                                  //| C), State(B,A)), List(State(B,D), State(B,C), State(D,C)), List(State(B,D),
                                                  //|  State(B,D), State(A,A)), List(State(B,D), State(B,D), State(C,C)), List(St
                                                  //| ate(B,D), State(B,D), State(A,D)), List(State(B,D), State(B,D), State(B,C))
                                                  //| , List(State(B,D), State(B,D), State(B,D)), List(State(B,D), State(B,D), St
                                                  //| ate(C,D)), List(State(B,D), State(B,D), State(B,A)), List(State(B,D), State
                                                  //| (C,D), State(A,A)), List(State(B,D), State(C,D), State(B,B)), List(State(B,
                                                  //| D), State(C,D), State(A,D)), List(State(B,D), State(C,D), State(B,D)), List
                                                  //| (State(B,D), State(C,D), State(C,D)), List(State(B,D), State(C,D), State(C,
                                                  //| A)), List(State(B,D), State(C,D), State(C,B)), List(State(B,D), State(B,A),
                                                  //|  State(C,C)), List(State(B,D), State(B,A), State(D,D)), List(State(B,D), St
                                                  //| ate(B,A), State(B,C)), List(State(B,D), State(B,A), State(B,D)), List(State
                                                  //| (B,D), State(B,A), State(B,A)), List(State(B,D), State(B,A), State(C,A)), L
                                                  //| ist(State(B,D), State(B,A), State(D,A)), List(State(C,D), State(A,A), State
                                                  //| (B,B)), List(State(C,D), State(A,A), State(C,C)), List(State(C,D), State(A,
                                                  //| A), State(D,D)), List(State(C,D), State(A,A), State(B,C)), List(State(C,D),
                                                  //|  State(A,A), State(B,D)), List(State(C,D), State(A,A), State(C,D)), List(St
                                                  //| ate(C,D), State(A,A), State(C,B)), List(State(C,D), State(A,A), State(D,B))
                                                  //| , List(State(C,D), State(A,A), State(D,C)), List(State(C,D), State(B,B), St
                                                  //| ate(A,A)), List(State(C,D), State(B,B), State(C,C)), List(State(C,D), State
                                                  //| (B,B), State(D,D)), List(State(C,D), State(B,B), State(A,C)), List(State(C,
                                                  //| D), State(B,B), State(A,D)), List(State(C,D), State(B,B), State(C,D)), List
                                                  //| (State(C,D), State(B,B), State(C,A)), List(State(C,D), State(B,B), State(D,
                                                  //| A)), List(State(C,D), State(B,B), State(D,C)), List(State(C,D), State(A,D),
                                                  //|  State(B,B)), List(State(C,D), State(A,D), State(C,C)), List(State(C,D), St
                                                  //| ate(A,D), State(A,B)), List(State(C,D), State(A,D), State(A,C)), List(State
                                                  //| (C,D), State(A,D), State(A,D)), List(State(C,D), State(A,D), State(B,D)), L
                                                  //| ist(State(C,D), State(A,D), State(C,D)), List(State(C,D), State(B,D), State
                                                  //| (A,A)), List(State(C,D), State(B,D), State(C,C)), List(State(C,D), State(B,
                                                  //| D), State(A,D)), List(State(C,D), State(B,D), State(B,C)), List(State(C,D),
                                                  //|  State(B,D), State(B,D)), List(State(C,D), State(B,D), State(C,D)), List(St
                                                  //| ate(C,D), State(B,D), State(B,A)), List(State(C,D), State(C,D), State(A,A))
                                                  //| , List(State(C,D), State(C,D), State(B,B)), List(State(C,D), State(C,D), St
                                                  //| ate(A,D)), List(State(C,D), State(C,D), State(B,D)), List(State(C,D), State
                                                  //| (C,D), State(C,D)), List(State(C,D), State(C,D), State(C,A)), List(State(C,
                                                  //| D), State(C,D), State(C,B)), List(State(C,D), State(C,A), State(B,B)), List
                                                  //| (State(C,D), State(C,A), State(D,D)), List(State(C,D), State(C,A), State(C,
                                                  //| D)), List(State(C,D), State(C,A), State(B,A)), List(State(C,D), State(C,A),
                                                  //|  State(C,A)), List(State(C,D), State(C,A), State(D,A)), List(State(C,D), St
                                                  //| ate(C,A), State(C,B)), List(State(C,D), State(C,B), State(A,A)), List(State
                                                  //| (C,D), State(C,B), State(D,D)), List(State(C,D), State(C,B), State(A,B)), L
                                                  //| ist(State(C,D), State(C,B), State(C,D)), List(State(C,D), State(C,B), State
                                                  //| (C,A)), List(State(C,D), State(C,B), State(C,B)), List(State(C,D), State(C,
                                                  //| B), State(D,B)), List(State(B,A), State(C,C), State(A,A)), List(State(B,A),
                                                  //|  State(C,C), State(B,B)), List(State(B,A), State(C,C), State(D,D)), List(St
                                                  //| ate(B,A), State(C,C), State(A,B)), List(State(B,A), State(C,C), State(A,D))
                                                  //| , List(State(B,A), State(C,C), State(B,D)), List(State(B,A), State(C,C), St
                                                  //| ate(B,A)), List(State(B,A), State(C,C), State(D,A)), List(State(B,A), State
                                                  //| (C,C), State(D,B)), List(State(B,A), State(D,D), State(A,A)), List(State(B,
                                                  //| A), State(D,D), State(B,B)), List(State(B,A), State(D,D), State(C,C)), List
                                                  //| (State(B,A), State(D,D), State(A,B)), List(State(B,A), State(D,D), State(A,
                                                  //| C)), List(State(B,A), State(D,D), State(B,C)), List(State(B,A), State(D,D),
                                                  //|  State(B,A)), List(State(B,A), State(D,D), State(C,A)), List(State(B,A), St
                                                  //| ate(D,D), State(C,B)), List(State(B,A), State(B,C), State(A,A)), List(State
                                                  //| (B,A), State(B,C), State(D,D)), List(State(B,A), State(B,C), State(A,C)), L
                                                  //| ist(State(B,A), State(B,C), State(B,C)), List(State(B,A), State(B,C), State
                                                  //| (B,D)), List(State(B,A), State(B,C), State(B,A)), List(State(B,A), State(B,
                                                  //| C), State(D,C)), List(State(B,A), State(B,D), State(A,A)), List(State(B,A),
                                                  //|  State(B,D), State(C,C)), List(State(B,A), State(B,D), State(A,D)), List(St
                                                  //| ate(B,A), State(B,D), State(B,C)), List(State(B,A), State(B,D), State(B,D))
                                                  //| , List(State(B,A), State(B,D), State(C,D)), List(State(B,A), State(B,D), St
                                                  //| ate(B,A)), List(State(B,A), State(B,A), State(C,C)), List(State(B,A), State
                                                  //| (B,A), State(D,D)), List(State(B,A), State(B,A), State(B,C)), List(State(B,
                                                  //| A), State(B,A), State(B,D)), List(State(B,A), State(B,A), State(B,A)), List
                                                  //| (State(B,A), State(B,A), State(C,A)), List(State(B,A), State(B,A), State(D,
                                                  //| A)), List(State(B,A), State(C,A), State(B,B)), List(State(B,A), State(C,A),
                                                  //|  State(D,D)), List(State(B,A), State(C,A), State(C,D)), List(State(B,A), St
                                                  //| ate(C,A), State(B,A)), List(State(B,A), State(C,A), State(C,A)), List(State
                                                  //| (B,A), State(C,A), State(D,A)), List(State(B,A), State(C,A), State(C,B)), L
                                                  //| ist(State(B,A), State(D,A), State(B,B)), List(State(B,A), State(D,A), State
                                                  //| (C,C)), List(State(B,A), State(D,A), State(B,A)), List(State(B,A), State(D,
                                                  //| A), State(C,A)), List(State(B,A), State(D,A), State(D,A)), List(State(B,A),
                                                  //|  State(D,A), State(D,B)), List(State(B,A), State(D,A), State(D,C)), List(St
                                                  //| ate(C,A), State(B,B), State(A,A)), List(State(C,A), State(B,B), State(C,C))
                                                  //| , List(State(C,A), State(B,B), State(D,D)), List(State(C,A), State(B,B), St
                                                  //| ate(A,C)), List(State(C,A), State(B,B), State(A,D)), List(State(C,A), State
                                                  //| (B,B), State(C,D)), List(State(C,A), State(B,B), State(C,A)), List(State(C,
                                                  //| A), State(B,B), State(D,A)), List(State(C,A), State(B,B), State(D,C)), List
                                                  //| (State(C,A), State(D,D), State(A,A)), List(State(C,A), State(D,D), State(B,
                                                  //| B)), List(State(C,A), State(D,D), State(C,C)), List(State(C,A), State(D,D),
                                                  //|  State(A,B)), List(State(C,A), State(D,D), State(A,C)), List(State(C,A), St
                                                  //| ate(D,D), State(B,C)), List(State(C,A), State(D,D), State(B,A)), List(State
                                                  //| (C,A), State(D,D), State(C,A)), List(State(C,A), State(D,D), State(C,B)), L
                                                  //| ist(State(C,A), State(C,D), State(A,A)), List(State(C,A), State(C,D), State
                                                  //| (B,B)), List(State(C,A), State(C,D), State(A,D)), List(State(C,A), State(C,
                                                  //| D), State(B,D)), List(State(C,A), State(C,D), State(C,D)), List(State(C,A),
                                                  //|  State(C,D), State(C,A)), List(State(C,A), State(C,D), State(C,B)), List(St
                                                  //| ate(C,A), State(B,A), State(C,C)), List(State(C,A), State(B,A), State(D,D))
                                                  //| , List(State(C,A), State(B,A), State(B,C)), List(State(C,A), State(B,A), St
                                                  //| ate(B,D)), List(State(C,A), State(B,A), State(B,A)), List(State(C,A), State
                                                  //| (B,A), State(C,A)), List(State(C,A), State(B,A), State(D,A)), List(State(C,
                                                  //| A), State(C,A), State(B,B)), List(State(C,A), State(C,A), State(D,D)), List
                                                  //| (State(C,A), State(C,A), State(C,D)), List(State(C,A), State(C,A), State(B,
                                                  //| A)), List(State(C,A), State(C,A), State(C,A)), List(State(C,A), State(C,A),
                                                  //|  State(D,A)), List(State(C,A), State(C,A), State(C,B)), List(State(C,A), St
                                                  //| ate(D,A), State(B,B)), List(State(C,A), State(D,A), State(C,C)), List(State
                                                  //| (C,A), State(D,A), State(B,A)), List(State(C,A), State(D,A), State(C,A)), L
                                                  //| ist(State(C,A), State(D,A), State(D,A)), List(State(C,A), State(D,A), State
                                                  //| (D,B)), List(State(C,A), State(D,A), State(D,C)), List(State(C,A), State(C,
                                                  //| B), State(A,A)), List(State(C,A), State(C,B), State(D,D)), List(State(C,A),
                                                  //|  State(C,B), State(A,B)), List(State(C,A), State(C,B), State(C,D)), List(St
                                                  //| ate(C,A), State(C,B), State(C,A)), List(State(C,A), State(C,B), State(C,B))
                                                  //| , List(State(C,A), State(C,B), State(D,B)), List(State(D,A), State(B,B), St
                                                  //| ate(A,A)), List(State(D,A), State(B,B), State(C,C)), List(State(D,A), State
                                                  //| (B,B), State(D,D)), List(State(D,A), State(B,B), State(A,C)), List(State(D,
                                                  //| A), State(B,B), State(A,D)), List(State(D,A), State(B,B), State(C,D)), List
                                                  //| (State(D,A), State(B,B), State(C,A)), List(State(D,A), State(B,B), State(D,
                                                  //| A)), List(State(D,A), State(B,B), State(D,C)), List(State(D,A), State(C,C),
                                                  //|  State(A,A)), List(State(D,A), State(C,C), State(B,B)), List(State(D,A), St
                                                  //| ate(C,C), State(D,D)), List(State(D,A), State(C,C), State(A,B)), List(State
                                                  //| (D,A), State(C,C), State(A,D)), List(State(D,A), State(C,C), State(B,D)), L
                                                  //| ist(State(D,A), State(C,C), State(B,A)), List(State(D,A), State(C,C), State
                                                  //| (D,A)), List(State(D,A), State(C,C), State(D,B)), List(State(D,A), State(B,
                                                  //| A), State(C,C)), List(State(D,A), State(B,A), State(D,D)), List(State(D,A),
                                                  //|  State(B,A), State(B,C)), List(State(D,A), State(B,A), State(B,D)), List(St
                                                  //| ate(D,A), State(B,A), State(B,A)), List(State(D,A), State(B,A), State(C,A))
                                                  //| , List(State(D,A), State(B,A), State(D,A)), List(State(D,A), State(C,A), St
                                                  //| ate(B,B)), List(State(D,A), State(C,A), State(D,D)), List(State(D,A), State
                                                  //| (C,A), State(C,D)), List(State(D,A), State(C,A), State(B,A)), List(State(D,
                                                  //| A), State(C,A), State(C,A)), List(State(D,A), State(C,A), State(D,A)), List
                                                  //| (State(D,A), State(C,A), State(C,B)), List(State(D,A), State(D,A), State(B,
                                                  //| B)), List(State(D,A), State(D,A), State(C,C)), List(State(D,A), State(D,A),
                                                  //|  State(B,A)), List(State(D,A), State(D,A), State(C,A)), List(State(D,A), St
                                                  //| ate(D,A), State(D,A)), List(State(D,A), State(D,A), State(D,B)), List(State
                                                  //| (D,A), State(D,A), State(D,C)), List(State(D,A), State(D,B), State(A,A)), L
                                                  //| ist(State(D,A), State(D,B), State(C,C)), List(State(D,A), State(D,B), State
                                                  //| (A,B)), List(State(D,A), State(D,B), State(D,A)), List(State(D,A), State(D,
                                                  //| B), State(C,B)), List(State(D,A), State(D,B), State(D,B)), List(State(D,A),
                                                  //|  State(D,B), State(D,C)), List(State(D,A), State(D,C), State(A,A)), List(St
                                                  //| ate(D,A), State(D,C), State(B,B)), List(State(D,A), State(D,C), State(A,C))
                                                  //| , List(State(D,A), State(D,C), State(B,C)), List(State(D,A), State(D,C), St
                                                  //| ate(D,A)), List(State(D,A), State(D,C), State(D,B)), List(State(D,A), State
                                                  //| (D,C), State(D,C)), List(State(C,B), State(A,A), State(B,B)), List(State(C,
                                                  //| B), State(A,A), State(C,C)), List(State(C,B), State(A,A), State(D,D)), List
                                                  //| (State(C,B), State(A,A), State(B,C)), List(State(C,B), State(A,A), State(B,
                                                  //| D)), List(State(C,B), State(A,A), State(C,D)), List(State(C,B), State(A,A),
                                                  //|  State(C,B)), List(State(C,B), State(A,A), State(D,B)), List(State(C,B), St
                                                  //| ate(A,A), State(D,C)), List(State(C,B), State(D,D), State(A,A)), List(State
                                                  //| (C,B), State(D,D), State(B,B)), List(State(C,B), State(D,D), State(C,C)), L
                                                  //| ist(State(C,B), State(D,D), State(A,B)), List(State(C,B), State(D,D), State
                                                  //| (A,C)), List(State(C,B), State(D,D), State(B,C)), List(State(C,B), State(D,
                                                  //| D), State(B,A)), List(State(C,B), State(D,D), State(C,A)), List(State(C,B),
                                                  //|  State(D,D), State(C,B)), List(State(C,B), State(A,B), State(C,C)), List(St
                                                  //| ate(C,B), State(A,B), State(D,D)), List(State(C,B), State(A,B), State(A,B))
                                                  //| , List(State(C,B), State(A,B), State(A,C)), List(State(C,B), State(A,B), St
                                                  //| ate(A,D)), List(State(C,B), State(A,B), State(C,B)), List(State(C,B), State
                                                  //| (A,B), State(D,B)), List(State(C,B), State(C,D), State(A,A)), List(State(C,
                                                  //| B), State(C,D), State(B,B)), List(State(C,B), State(C,D), State(A,D)), List
                                                  //| (State(C,B), State(C,D), State(B,D)), List(State(C,B), State(C,D), State(C,
                                                  //| D)), List(State(C,B), State(C,D), State(C,A)), List(State(C,B), State(C,D),
                                                  //|  State(C,B)), List(State(C,B), State(C,A), State(B,B)), List(State(C,B), St
                                                  //| ate(C,A), State(D,D)), List(State(C,B), State(C,A), State(C,D)), List(State
                                                  //| (C,B), State(C,A), State(B,A)), List(State(C,B), State(C,A), State(C,A)), L
                                                  //| ist(State(C,B), State(C,A), State(D,A)), List(State(C,B), State(C,A), State
                                                  //| (C,B)), List(State(C,B), State(C,B), State(A,A)), List(State(C,B), State(C,
                                                  //| B), State(D,D)), List(State(C,B), State(C,B), State(A,B)), List(State(C,B),
                                                  //|  State(C,B), State(C,D)), List(State(C,B), State(C,B), State(C,A)), List(St
                                                  //| ate(C,B), State(C,B), State(C,B)), List(State(C,B), State(C,B), State(D,B))
                                                  //| , List(State(C,B), State(D,B), State(A,A)), List(State(C,B), State(D,B), St
                                                  //| ate(C,C)), List(State(C,B), State(D,B), State(A,B)), List(State(C,B), State
                                                  //| (D,B), State(D,A)), List(State(C,B), State(D,B), State(C,B)), List(State(C,
                                                  //| B), State(D,B), State(D,B)), List(State(C,B), State(D,B), State(D,C)), List
                                                  //| (State(D,B), State(A,A), State(B,B)), List(State(D,B), State(A,A), State(C,
                                                  //| C)), List(State(D,B), State(A,A), State(D,D)), List(State(D,B), State(A,A),
                                                  //|  State(B,C)), List(State(D,B), State(A,A), State(B,D)), List(State(D,B), St
                                                  //| ate(A,A), State(C,D)), List(State(D,B), State(A,A), State(C,B)), List(State
                                                  //| (D,B), State(A,A), State(D,B)), List(State(D,B), State(A,A), State(D,C)), L
                                                  //| ist(State(D,B), State(C,C), State(A,A)), List(State(D,B), State(C,C), State
                                                  //| (B,B)), List(State(D,B), State(C,C), State(D,D)), List(State(D,B), State(C,
                                                  //| C), State(A,B)), List(State(D,B), State(C,C), State(A,D)), List(State(D,B),
                                                  //|  State(C,C), State(B,D)), List(State(D,B), State(C,C), State(B,A)), List(St
                                                  //| ate(D,B), State(C,C), State(D,A)), List(State(D,B), State(C,C), State(D,B))
                                                  //| , List(State(D,B), State(A,B), State(C,C)), List(State(D,B), State(A,B), St
                                                  //| ate(D,D)), List(State(D,B), State(A,B), State(A,B)), List(State(D,B), State
                                                  //| (A,B), State(A,C)), List(State(D,B), State(A,B), State(A,D)), List(State(D,
                                                  //| B), State(A,B), State(C,B)), List(State(D,B), State(A,B), State(D,B)), List
                                                  //| (State(D,B), State(D,A), State(B,B)), List(State(D,B), State(D,A), State(C,
                                                  //| C)), List(State(D,B), State(D,A), State(B,A)), List(State(D,B), State(D,A),
                                                  //|  State(C,A)), List(State(D,B), State(D,A), State(D,A)), List(State(D,B), St
                                                  //| ate(D,A), State(D,B)), List(State(D,B), State(D,A), State(D,C)), List(State
                                                  //| (D,B), State(C,B), State(A,A)), List(State(D,B), State(C,B), State(D,D)), L
                                                  //| ist(State(D,B), State(C,B), State(A,B)), List(State(D,B), State(C,B), State
                                                  //| (C,D)), List(State(D,B), State(C,B), State(C,A)), List(State(D,B), State(C,
                                                  //| B), State(C,B)), List(State(D,B), State(C,B), State(D,B)), List(State(D,B),
                                                  //|  State(D,B), State(A,A)), List(State(D,B), State(D,B), State(C,C)), List(St
                                                  //| ate(D,B), State(D,B), State(A,B)), List(State(D,B), State(D,B), State(D,A))
                                                  //| , List(State(D,B), State(D,B), State(C,B)), List(State(D,B), State(D,B), St
                                                  //| ate(D,B)), List(State(D,B), State(D,B), State(D,C)), List(State(D,B), State
                                                  //| (D,C), State(A,A)), List(State(D,B), State(D,C), State(B,B)), List(State(D,
                                                  //| B), State(D,C), State(A,C)), List(State(D,B), State(D,C), State(B,C)), List
                                                  //| (State(D,B), State(D,C), State(D,A)), List(State(D,B), State(D,C), State(D,
                                                  //| B)), List(State(D,B), State(D,C), State(D,C)), List(State(D,C), State(A,A),
                                                  //|  State(B,B)), List(State(D,C), State(A,A), State(C,C)), List(State(D,C), St
                                                  //| ate(A,A), State(D,D)), List(State(D,C), State(A,A), State(B,C)), List(State
                                                  //| (D,C), State(A,A), State(B,D)), List(State(D,C), State(A,A), State(C,D)), L
                                                  //| ist(State(D,C), State(A,A), State(C,B)), List(State(D,C), State(A,A), State
                                                  //| (D,B)), List(State(D,C), State(A,A), State(D,C)), List(State(D,C), State(B,
                                                  //| B), State(A,A)), List(State(D,C), State(B,B), State(C,C)), List(State(D,C),
                                                  //|  State(B,B), State(D,D)), List(State(D,C), State(B,B), State(A,C)), List(St
                                                  //| ate(D,C), State(B,B), State(A,D)), List(State(D,C), State(B,B), State(C,D))
                                                  //| , List(State(D,C), State(B,B), State(C,A)), List(State(D,C), State(B,B), St
                                                  //| ate(D,A)), List(State(D,C), State(B,B), State(D,C)), List(State(D,C), State
                                                  //| (A,C), State(B,B)), List(State(D,C), State(A,C), State(D,D)), List(State(D,
                                                  //| C), State(A,C), State(A,B)), List(State(D,C), State(A,C), State(A,C)), List
                                                  //| (State(D,C), State(A,C), State(A,D)), List(State(D,C), State(A,C), State(B,
                                                  //| C)), List(State(D,C), State(A,C), State(D,C)), List(State(D,C), State(B,C),
                                                  //|  State(A,A)), List(State(D,C), State(B,C), State(D,D)), List(State(D,C), St
                                                  //| ate(B,C), State(A,C)), List(State(D,C), State(B,C), State(B,C)), List(State
                                                  //| (D,C), State(B,C), State(B,D)), List(State(D,C), State(B,C), State(B,A)), L
                                                  //| ist(State(D,C), State(B,C), State(D,C)), List(State(D,C), State(D,A), State
                                                  //| (B,B)), List(State(D,C), State(D,A), State(C,C)), List(State(D,C), State(D,
                                                  //| A), State(B,A)), List(State(D,C), State(D,A), State(C,A)), List(State(D,C),
                                                  //|  State(D,A), State(D,A)), List(State(D,C), State(D,A), State(D,B)), List(St
                                                  //| ate(D,C), State(D,A), State(D,C)), List(State(D,C), State(D,B), State(A,A))
                                                  //| , List(State(D,C), State(D,B), State(C,C)), List(State(D,C), State(D,B), St
                                                  //| ate(A,B)), List(State(D,C), State(D,B), State(D,A)), List(State(D,C), State
                                                  //| (D,B), State(C,B)), List(State(D,C), State(D,B), State(D,B)), List(State(D,
                                                  //| C), State(D,B), State(D,C)), List(State(D,C), State(D,C), State(A,A)), List
                                                  //| (State(D,C), State(D,C), State(B,B)), List(State(D,C), State(D,C), State(A,
                                                  //| C)), List(State(D,C), State(D,C), State(B,C)), List(State(D,C), State(D,C),
                                                  //|  State(D,A)), List(State(D,C), State(D,C), State(D,B)), List(State(D,C), St
                                                  //| ate(D,C), State(D,C)))
    val three_States_Map = three_States.toArray.map { routine =>
      generate16States.filter(stateToAppend => doesntViolateAnyRules(routine, stateToAppend))
        .map(stateToAppend => three_States.indexOf(routine.drop(1) :+ stateToAppend))
    }                                             //> three_States_Map  : Array[List[Int]] = Array(List(69, 70, 71, 72, 73, 74, 7
                                                  //| 5, 76, 77), List(78, 79, 80, 81, 82, 83, 84, 85, 86), List(87, 88, 89, 90, 
                                                  //| 91, 92, 93, 94, 95), List(96, 97, 98, 99, 100, 101, 102), List(103, 104, 10
                                                  //| 5, 106, 107, 108, 109), List(110, 111, 112, 113, 114, 115, 116), List(117, 
                                                  //| 118, 119, 120, 121, 122, 123), List(124, 125, 126, 127, 128, 129, 130), Lis
                                                  //| t(131, 132, 133, 134, 135, 136, 137), List(138, 139, 140, 141, 142, 143, 14
                                                  //| 4, 145, 146), List(147, 148, 149, 150, 151, 152, 153, 154, 155), List(156, 
                                                  //| 157, 158, 159, 160, 161, 162, 163, 164), List(165, 166, 167, 168, 169, 170,
                                                  //|  171), List(172, 173, 174, 175, 176, 177, 178), List(179, 180, 181, 182, 18
                                                  //| 3, 184, 185), List(186, 187, 188, 189, 190, 191, 192), List(193, 194, 195, 
                                                  //| 196, 197, 198, 199), List(200, 201, 202, 203, 204, 205, 206), List(207, 208
                                                  //| , 209, 210, 211, 212, 213, 214, 215), List(216, 217, 218, 219, 220, 221, 22
                                                  //| 2, 223, 224), List(225, 226, 227, 228, 229, 230, 231, 232, 233), List(234, 
                                                  //| 235, 236, 237, 238, 239, 240), List(241, 242, 243, 244, 245, 246, 247), Lis
                                                  //| t(248, 249, 250, 251, 252, 253, 254), List(255, 256, 257, 258, 259, 260, 26
                                                  //| 1), List(262, 263, 264, 265, 266, 267, 268), List(269, 270, 271, 272, 273, 
                                                  //| 274, 275), List(435, 436, 437, 438, 439, 440, 441, 442, 443), List(444, 445
                                                  //| , 446, 447, 448, 449, 450, 451, 452), List(453, 454, 455, 456, 457, 458, 45
                                                  //| 9), List(460, 461, 462, 463, 464, 465, 466), List(467, 468, 469, 470, 471, 
                                                  //| 472, 473), List(474, 475, 476, 477, 478, 479, 480), List(481, 482, 483, 484
                                                  //| , 485, 486, 487), List(488, 489, 490, 491, 492, 493, 494, 495, 496), List(4
                                                  //| 97, 498, 499, 500, 501, 502, 503, 504, 505), List(506, 507, 508, 509, 510, 
                                                  //| 511, 512), List(513, 514, 515, 516, 517, 518, 519), List(520, 521, 522, 523
                                                  //| , 524, 525, 526), List(527, 528, 529, 530, 531, 532, 533), List(534, 535, 5
                                                  //| 36, 537, 538, 539, 540), List(541, 542, 543, 544, 545, 546, 547, 548, 549),
                                                  //|  List(550, 551, 552, 553, 554, 555, 556, 557, 558), List(559, 560, 561, 562
                                                  //| , 563, 564, 565), List(566, 567, 568, 569, 570, 571, 572), List(573, 574, 5
                                                  //| 75, 576, 577, 578, 579), List(580, 581, 582, 583, 584, 585, 586), List(587,
                                                  //|  588, 589, 590, 591, 592, 593), List(753, 754, 755, 756, 757, 758, 759, 760
                                                  //| , 761), List(762, 763, 764, 765, 766, 767, 768, 769, 770), List(771, 772, 7
                                                  //| 73, 774, 775, 776, 777), List(778, 779, 780, 781, 782, 783, 784), List(785,
                                                  //|  786, 787, 788, 789, 790, 791), List(792, 793, 794, 795, 796, 797, 798), Li
                                                  //| st(799, 800, 801, 802, 803, 804, 805), List(806, 807, 808, 809, 810, 811, 8
                                                  //| 12, 813, 814), List(815, 816, 817, 818, 819, 820, 821, 822, 823), List(824,
                                                  //|  825, 826, 827, 828, 829, 830), List(831, 832, 833, 834, 835, 836, 837), Li
                                                  //| st(838, 839, 840, 841, 842, 843, 844), List(845, 846, 847, 848, 849, 850, 8
                                                  //| 51), List(852, 853, 854, 855, 856, 857, 858), List(859, 860, 861, 862, 863,
                                                  //|  864, 865, 866, 867), List(868, 869, 870, 871, 872, 873, 874, 875, 876), Li
                                                  //| st(877, 878, 879, 880, 881, 882, 883), List(884, 885, 886, 887, 888, 889, 8
                                                  //| 90), List(891, 892, 893, 894, 895, 896, 897), List(898, 899, 900, 901, 902,
                                                  //|  903, 904), List(905, 906, 907, 908, 909, 910, 911), List(0, 1, 2, 3, 4, 5,
                                                  //|  6, 7, 8), List(9, 10, 11, 12, 13, 14, 15, 16, 17), List(18, 19, 20, 21, 22
                                                  //| , 23, 24, 25, 26), List(27, 28, 29, 30, 31, 32, 33), List(34, 35, 36, 37, 3
                                                  //| 8, 39, 40), List(41, 42, 43, 44, 45, 46, 47), List(48, 49, 50, 51, 52, 53, 
                                                  //| 54), List(55, 56, 57, 58, 59, 60, 61), List(62, 63, 64, 65, 66, 67, 68), Li
                                                  //| st(138, 139, 140, 141, 142, 143, 144, 145, 146), List(147, 148, 149, 150, 1
                                                  //| 51, 152, 153, 154, 155), List(156, 157, 158, 159, 160, 161, 162, 163, 164),
                                                  //|  List(165, 166, 167, 168, 169, 170, 171), List(172, 173, 174, 175, 176, 177
                                                  //| , 178), List(179, 180, 181, 182, 183, 184, 185), List(186, 187, 188, 189, 1
                                                  //| 90, 191, 192), List(193, 194, 195, 196, 197, 198, 199), List(200, 201, 202,
                                                  //|  203, 204, 205, 206), List(207, 208, 209, 210, 211, 212, 213, 214, 215), Li
                                                  //| st(216, 217, 218, 219, 220, 221, 222, 223, 224), List(225, 226, 227, 228, 2
                                                  //| 29, 230, 231, 232, 233), List(234, 235, 236, 237, 238, 239, 240), List(241,
                                                  //|  242, 243, 244, 245, 246, 247), List(248, 249, 250, 251, 252, 253, 254), Li
                                                  //| st(255, 256, 257, 258, 259, 260, 261), List(262, 263, 264, 265, 266, 267, 2
                                                  //| 68), List(269, 270, 271, 272, 273, 274, 275), List(329, 330, 331, 332, 333,
                                                  //|  334, 335, 336, 337), List(338, 339, 340, 341, 342, 343, 344, 345, 346), Li
                                                  //| st(347, 348, 349, 350, 351, 352, 353), List(354, 355, 356, 357, 358, 359, 3
                                                  //| 60), List(361, 362, 363, 364, 365, 366, 367), List(368, 369, 370, 371, 372,
                                                  //|  373, 374), List(375, 376, 377, 378, 379, 380, 381), List(382, 383, 384, 38
                                                  //| 5, 386, 387, 388, 389, 390), List(391, 392, 393, 394, 395, 396, 397, 398, 3
                                                  //| 99), List(400, 401, 402, 403, 404, 405, 406), List(407, 408, 409, 410, 411,
                                                  //|  412, 413), List(414, 415, 416, 417, 418, 419, 420), List(421, 422, 423, 42
                                                  //| 4, 425, 426, 427), List(428, 429, 430, 431, 432, 433, 434), List(541, 542, 
                                                  //| 543, 544, 545, 546, 547, 548, 549), List(550, 551, 552, 553, 554, 555, 556,
                                                  //|  557, 558), List(559, 560, 561, 562, 563, 564, 565), List(566, 567, 568, 56
                                                  //| 9, 570, 571, 572), List(573, 574, 575, 576, 577, 578, 579), List(580, 581, 
                                                  //| 582, 583, 584, 585, 586), List(587, 588, 589, 590, 591, 592, 593), List(647
                                                  //| , 648, 649, 650, 651, 652, 653, 654, 655), List(656, 657, 658, 659, 660, 66
                                                  //| 1, 662, 663, 664), List(665, 666, 667, 668, 669, 670, 671), List(672, 673, 
                                                  //| 674, 675, 676, 677, 678), List(679, 680, 681, 682, 683, 684, 685), List(686
                                                  //| , 687, 688, 689, 690, 691, 692), List(693, 694, 695, 696, 697, 698, 699), L
                                                  //| ist(700, 701, 702, 703, 704, 705, 706, 707, 708), List(709, 710, 711, 712, 
                                                  //| 713, 714, 715, 716, 717), List(718, 719, 720, 721, 722, 723, 724), List(725
                                                  //| , 726, 727, 728, 729, 730, 731), List(732, 733, 734, 735, 736, 737, 738), L
                                                  //| ist(739, 740, 741, 742, 743, 744, 745), List(746, 747, 748, 749, 750, 751, 
                                                  //| 752), List(859, 860, 861, 862, 863, 864, 865, 866, 867), List(868, 869, 870
                                                  //| , 871, 872, 873, 874, 875, 876), List(877, 878, 879, 880, 881, 882, 883), L
                                                  //| ist(884, 885, 886, 887, 888, 889, 890), List(891, 892, 893, 894, 895, 896, 
                                                  //| 897), List(898, 899, 900, 901, 902, 903, 904), List(905, 906, 907, 908, 909
                                                  //| , 910, 911), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14, 1
                                                  //| 5, 16, 17), List(18, 19, 20, 21, 22, 23, 24, 25, 26), List(27, 28, 29, 30, 
                                                  //| 31, 32, 33), List(34, 35, 36, 37, 38, 39, 40), List(41, 42, 43, 44, 45, 46,
                                                  //|  47), List(48, 49, 50, 51, 52, 53, 54), List(55, 56, 57, 58, 59, 60, 61), L
                                                  //| ist(62, 63, 64, 65, 66, 67, 68), List(69, 70, 71, 72, 73, 74, 75, 76, 77), 
                                                  //| List(78, 79, 80, 81, 82, 83, 84, 85, 86), List(87, 88, 89, 90, 91, 92, 93, 
                                                  //| 94, 95), List(96, 97, 98, 99, 100, 101, 102), List(103, 104, 105, 106, 107,
                                                  //|  108, 109), List(110, 111, 112, 113, 114, 115, 116), List(117, 118, 119, 12
                                                  //| 0, 121, 122, 123), List(124, 125, 126, 127, 128, 129, 130), List(131, 132, 
                                                  //| 133, 134, 135, 136, 137), List(207, 208, 209, 210, 211, 212, 213, 214, 215)
                                                  //| , List(216, 217, 218, 219, 220, 221, 222, 223, 224), List(225, 226, 227, 22
                                                  //| 8, 229, 230, 231, 232, 233), List(234, 235, 236, 237, 238, 239, 240), List(
                                                  //| 241, 242, 243, 244, 245, 246, 247), List(248, 249, 250, 251, 252, 253, 254)
                                                  //| , List(255, 256, 257, 258, 259, 260, 261), List(262, 263, 264, 265, 266, 26
                                                  //| 7, 268), List(269, 270, 271, 272, 273, 274, 275), List(276, 277, 278, 279, 
                                                  //| 280, 281, 282, 283, 284), List(285, 286, 287, 288, 289, 290, 291, 292, 293)
                                                  //| , List(294, 295, 296, 297, 298, 299, 300), List(301, 302, 303, 304, 305, 30
                                                  //| 6, 307), List(308, 309, 310, 311, 312, 313, 314), List(315, 316, 317, 318, 
                                                  //| 319, 320, 321), List(322, 323, 324, 325, 326, 327, 328), List(382, 383, 384
                                                  //| , 385, 386, 387, 388, 389, 390), List(391, 392, 393, 394, 395, 396, 397, 39
                                                  //| 8, 399), List(400, 401, 402, 403, 404, 405, 406), List(407, 408, 409, 410, 
                                                  //| 411, 412, 413), List(414, 415, 416, 417, 418, 419, 420), List(421, 422, 423
                                                  //| , 424, 425, 426, 427), List(428, 429, 430, 431, 432, 433, 434), List(488, 4
                                                  //| 89, 490, 491, 492, 493, 494, 495, 496), List(497, 498, 499, 500, 501, 502, 
                                                  //| 503, 504, 505), List(506, 507, 508, 509, 510, 511, 512), List(513, 514, 515
                                                  //| , 516, 517, 518, 519), List(520, 521, 522, 523, 524, 525, 526), List(527, 5
                                                  //| 28, 529, 530, 531, 532, 533), List(534, 535, 536, 537, 538, 539, 540), List
                                                  //| (594, 595, 596, 597, 598, 599, 600, 601, 602), List(603, 604, 605, 606, 607
                                                  //| , 608, 609, 610, 611), List(612, 613, 614, 615, 616, 617, 618), List(619, 6
                                                  //| 20, 621, 622, 623, 624, 625), List(626, 627, 628, 629, 630, 631, 632), List
                                                  //| (633, 634, 635, 636, 637, 638, 639), List(640, 641, 642, 643, 644, 645, 646
                                                  //| ), List(700, 701, 702, 703, 704, 705, 706, 707, 708), List(709, 710, 711, 7
                                                  //| 12, 713, 714, 715, 716, 717), List(718, 719, 720, 721, 722, 723, 724), List
                                                  //| (725, 726, 727, 728, 729, 730, 731), List(732, 733, 734, 735, 736, 737, 738
                                                  //| ), List(739, 740, 741, 742, 743, 744, 745), List(746, 747, 748, 749, 750, 7
                                                  //| 51, 752), List(806, 807, 808, 809, 810, 811, 812, 813, 814), List(815, 816,
                                                  //|  817, 818, 819, 820, 821, 822, 823), List(824, 825, 826, 827, 828, 829, 830
                                                  //| ), List(831, 832, 833, 834, 835, 836, 837), List(838, 839, 840, 841, 842, 8
                                                  //| 43, 844), List(845, 846, 847, 848, 849, 850, 851), List(852, 853, 854, 855,
                                                  //|  856, 857, 858), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 1
                                                  //| 4, 15, 16, 17), List(18, 19, 20, 21, 22, 23, 24, 25, 26), List(27, 28, 29, 
                                                  //| 30, 31, 32, 33), List(34, 35, 36, 37, 38, 39, 40), List(41, 42, 43, 44, 45,
                                                  //|  46, 47), List(48, 49, 50, 51, 52, 53, 54), List(55, 56, 57, 58, 59, 60, 61
                                                  //| ), List(62, 63, 64, 65, 66, 67, 68), List(69, 70, 71, 72, 73, 74, 75, 76, 7
                                                  //| 7), List(78, 79, 80, 81, 82, 83, 84, 85, 86), List(87, 88, 89, 90, 91, 92, 
                                                  //| 93, 94, 95), List(96, 97, 98, 99, 100, 101, 102), List(103, 104, 105, 106, 
                                                  //| 107, 108, 109), List(110, 111, 112, 113, 114, 115, 116), List(117, 118, 119
                                                  //| , 120, 121, 122, 123), List(124, 125, 126, 127, 128, 129, 130), List(131, 1
                                                  //| 32, 133, 134, 135, 136, 137), List(138, 139, 140, 141, 142, 143, 144, 145, 
                                                  //| 146), List(147, 148, 149, 150, 151, 152, 153, 154, 155), List(156, 157, 158
                                                  //| , 159, 160, 161, 162, 163, 164), List(165, 166, 167, 168, 169, 170, 171), L
                                                  //| ist(172, 173, 174, 175, 176, 177, 178), List(179, 180, 181, 182, 183, 184, 
                                                  //| 185), List(186, 187, 188, 189, 190, 191, 192), List(193, 194, 195, 196, 197
                                                  //| , 198, 199), List(200, 201, 202, 203, 204, 205, 206), List(276, 277, 278, 2
                                                  //| 79, 280, 281, 282, 283, 284), List(285, 286, 287, 288, 289, 290, 291, 292, 
                                                  //| 293), List(294, 295, 296, 297, 298, 299, 300), List(301, 302, 303, 304, 305
                                                  //| , 306, 307), List(308, 309, 310, 311, 312, 313, 314), List(315, 316, 317, 3
                                                  //| 18, 319, 320, 321), List(322, 323, 324, 325, 326, 327, 328), List(329, 330,
                                                  //|  331, 332, 333, 334, 335, 336, 337), List(338, 339, 340, 341, 342, 343, 344
                                                  //| , 345, 346), List(347, 348, 349, 350, 351, 352, 353), List(354, 355, 356, 3
                                                  //| 57, 358, 359, 360), List(361, 362, 363, 364, 365, 366, 367), List(368, 369,
                                                  //|  370, 371, 372, 373, 374), List(375, 376, 377, 378, 379, 380, 381), List(43
                                                  //| 5, 436, 437, 438, 439, 440, 441, 442, 443), List(444, 445, 446, 447, 448, 4
                                                  //| 49, 450, 451, 452), List(453, 454, 455, 456, 457, 458, 459), List(460, 461,
                                                  //|  462, 463, 464, 465, 466), List(467, 468, 469, 470, 471, 472, 473), List(47
                                                  //| 4, 475, 476, 477, 478, 479, 480), List(481, 482, 483, 484, 485, 486, 487), 
                                                  //| List(594, 595, 596, 597, 598, 599, 600, 601, 602), List(603, 604, 605, 606,
                                                  //|  607, 608, 609, 610, 611), List(612, 613, 614, 615, 616, 617, 618), List(61
                                                  //| 9, 620, 621, 622, 623, 624, 625), List(626, 627, 628, 629, 630, 631, 632), 
                                                  //| List(633, 634, 635, 636, 637, 638, 639), List(640, 641, 642, 643, 644, 645,
                                                  //|  646), List(647, 648, 649, 650, 651, 652, 653, 654, 655), List(656, 657, 65
                                                  //| 8, 659, 660, 661, 662, 663, 664), List(665, 666, 667, 668, 669, 670, 671), 
                                                  //| List(672, 673, 674, 675, 676, 677, 678), List(679, 680, 681, 682, 683, 684,
                                                  //|  685), List(686, 687, 688, 689, 690, 691, 692), List(693, 694, 695, 696, 69
                                                  //| 7, 698, 699), List(753, 754, 755, 756, 757, 758, 759, 760, 761), List(762, 
                                                  //| 763, 764, 765, 766, 767, 768, 769, 770), List(771, 772, 773, 774, 775, 776,
                                                  //|  777), List(778, 779, 780, 781, 782, 783, 784), List(785, 786, 787, 788, 78
                                                  //| 9, 790, 791), List(792, 793, 794, 795, 796, 797, 798), List(799, 800, 801, 
                                                  //| 802, 803, 804, 805), List(138, 139, 140, 141, 142, 143, 144, 145, 146), Lis
                                                  //| t(147, 148, 149, 150, 151, 152, 153, 154, 155), List(156, 157, 158, 159, 16
                                                  //| 0, 161, 162, 163, 164), List(165, 166, 167, 168, 169, 170, 171), List(172, 
                                                  //| 173, 174, 175, 176, 177, 178), List(179, 180, 181, 182, 183, 184, 185), Lis
                                                  //| t(186, 187, 188, 189, 190, 191, 192), List(193, 194, 195, 196, 197, 198, 19
                                                  //| 9), List(200, 201, 202, 203, 204, 205, 206), List(207, 208, 209, 210, 211, 
                                                  //| 212, 213, 214, 215), List(216, 217, 218, 219, 220, 221, 222, 223, 224), Lis
                                                  //| t(225, 226, 227, 228, 229, 230, 231, 232, 233), List(234, 235, 236, 237, 23
                                                  //| 8, 239, 240), List(241, 242, 243, 244, 245, 246, 247), List(248, 249, 250, 
                                                  //| 251, 252, 253, 254), List(255, 256, 257, 258, 259, 260, 261), List(262, 263
                                                  //| , 264, 265, 266, 267, 268), List(269, 270, 271, 272, 273, 274, 275), List(2
                                                  //| 76, 277, 278, 279, 280, 281, 282, 283, 284), List(285, 286, 287, 288, 289, 
                                                  //| 290, 291, 292, 293), List(294, 295), List(301, 302, 306, 307), List(308, 30
                                                  //| 9, 313, 314), List(315, 316, 318, 319), List(322, 323, 325, 328), List(329,
                                                  //|  330, 331, 332, 333, 334, 335, 336, 337), List(338, 339, 340, 341, 342, 343
                                                  //| , 344, 345, 346), List(347, 348, 352, 353), List(354, 355, 359, 360), List(
                                                  //| 361, 362, 366, 367), List(368, 369, 370, 371, 372, 373, 374), List(375, 376
                                                  //| , 377, 378, 379, 380, 381), List(382, 383, 384, 385, 386, 387, 388, 389, 39
                                                  //| 0), List(391, 392, 393, 394, 395, 396, 397, 398, 399), List(400, 401, 405, 
                                                  //| 406), List(407, 408, 412, 413), List(414, 415, 419, 420), List(421, 422, 42
                                                  //| 3, 424, 425, 426, 427), List(428, 429, 430, 431, 432, 433, 434), List(753, 
                                                  //| 754, 755, 756, 757, 758, 759, 760, 761), List(762, 763, 764, 765, 766, 767,
                                                  //|  768, 769, 770), List(771, 772, 774, 775), List(778, 779, 780, 781, 782, 78
                                                  //| 3, 784), List(785, 786, 787, 788, 789, 790, 791), List(792, 793, 795, 796),
                                                  //|  List(799, 800, 802, 805), List(806, 807, 808, 809, 810, 811, 812, 813, 814
                                                  //| ), List(815, 816, 817, 818, 819, 820, 821, 822, 823), List(824, 825, 827, 8
                                                  //| 28), List(831, 832, 833, 834, 835, 836, 837), List(838, 839, 841, 842), Lis
                                                  //| t(845, 846, 848, 851), List(852, 853, 854, 855, 856, 857, 858), List(69, 70
                                                  //| , 71, 72, 73, 74, 75, 76, 77), List(78, 79, 80, 81, 82, 83, 84, 85, 86), Li
                                                  //| st(87, 88, 89, 90, 91, 92, 93, 94, 95), List(96, 97, 98, 99, 100, 101, 102)
                                                  //| , List(103, 104, 105, 106, 107, 108, 109), List(110, 111, 112, 113, 114, 11
                                                  //| 5, 116), List(117, 118, 119, 120, 121, 122, 123), List(124, 125, 126, 127, 
                                                  //| 128, 129, 130), List(131, 132, 133, 134, 135, 136, 137), List(207, 208, 209
                                                  //| , 210, 211, 212, 213, 214, 215), List(216, 217, 218, 219, 220, 221, 222, 22
                                                  //| 3, 224), List(225, 226, 227, 228, 229, 230, 231, 232, 233), List(234, 235, 
                                                  //| 236, 237, 238, 239, 240), List(241, 242, 243, 244, 245, 246, 247), List(248
                                                  //| , 249, 250, 251, 252, 253, 254), List(255, 256, 257, 258, 259, 260, 261), L
                                                  //| ist(262, 263, 264, 265, 266, 267, 268), List(269, 270, 271, 272, 273, 274, 
                                                  //| 275), List(276, 277, 278, 279, 280, 281, 282, 283, 284), List(285, 286, 287
                                                  //| , 288, 289, 290, 291, 292, 293), List(294, 295, 299, 300), List(301, 302, 3
                                                  //| 06, 307), List(308, 309, 313, 314), List(315, 316, 317, 318, 319, 320, 321)
                                                  //| , List(322, 323, 324, 325, 326, 327, 328), List(329, 330, 331, 332, 333, 33
                                                  //| 4, 335, 336, 337), List(338, 339, 340, 341, 342, 343, 344, 345, 346), List(
                                                  //| 347, 348, 352, 353), List(354, 355), List(361, 362, 366, 367), List(368, 36
                                                  //| 9, 372, 373), List(375, 376, 379, 380), List(382, 383, 384, 385, 386, 387, 
                                                  //| 388, 389, 390), List(391, 392, 393, 394, 395, 396, 397, 398, 399), List(400
                                                  //| , 401, 405, 406), List(407, 408, 412, 413), List(414, 415, 419, 420), List(
                                                  //| 421, 422, 423, 424, 425, 426, 427), List(428, 429, 430, 431, 432, 433, 434)
                                                  //| , List(435, 436, 437, 438, 439, 440, 441, 442, 443), List(444, 445, 446, 44
                                                  //| 7, 448, 449, 450, 451, 452), List(453, 454, 455, 457), List(460, 461, 464, 
                                                  //| 465), List(467, 468, 469, 470, 471, 472, 473), List(474, 475, 476, 477, 478
                                                  //| , 479, 480), List(481, 482, 485, 486), List(859, 860, 861, 862, 863, 864, 8
                                                  //| 65, 866, 867), List(868, 869, 870, 871, 872, 873, 874, 875, 876), List(877,
                                                  //|  878, 879, 881), List(884, 885, 888, 889), List(891, 892, 893, 894, 895, 89
                                                  //| 6, 897), List(898, 899, 900, 901, 902, 903, 904), List(905, 906, 909, 910),
                                                  //|  List(69, 70, 71, 72, 73, 74, 75, 76, 77), List(78, 79, 80, 81, 82, 83, 84,
                                                  //|  85, 86), List(87, 88, 89, 90, 91, 92, 93, 94, 95), List(96, 97, 98, 99, 10
                                                  //| 0, 101, 102), List(103, 104, 105, 106, 107, 108, 109), List(110, 111, 112, 
                                                  //| 113, 114, 115, 116), List(117, 118, 119, 120, 121, 122, 123), List(124, 125
                                                  //| , 126, 127, 128, 129, 130), List(131, 132, 133, 134, 135, 136, 137), List(1
                                                  //| 38, 139, 140, 141, 142, 143, 144, 145, 146), List(147, 148, 149, 150, 151, 
                                                  //| 152, 153, 154, 155), List(156, 157, 158, 159, 160, 161, 162, 163, 164), Lis
                                                  //| t(165, 166, 167, 168, 169, 170, 171), List(172, 173, 174, 175, 176, 177, 17
                                                  //| 8), List(179, 180, 181, 182, 183, 184, 185), List(186, 187, 188, 189, 190, 
                                                  //| 191, 192), List(193, 194, 195, 196, 197, 198, 199), List(200, 201, 202, 203
                                                  //| , 204, 205, 206), List(276, 277, 278, 279, 280, 281, 282, 283, 284), List(2
                                                  //| 85, 286, 287, 288, 289, 290, 291, 292, 293), List(294, 295, 299, 300), List
                                                  //| (301, 302, 306, 307), List(308, 309, 313, 314), List(315, 316, 317, 318, 31
                                                  //| 9, 320, 321), List(322, 323, 324, 325, 326, 327, 328), List(329, 330, 331, 
                                                  //| 332, 333, 334, 335, 336, 337), List(338, 339, 340, 341, 342, 343, 344, 345,
                                                  //|  346), List(347, 348, 352, 353), List(354, 355, 359, 360), List(361, 362, 3
                                                  //| 66, 367), List(368, 369, 370, 371, 372, 373, 374), List(375, 376, 377, 378,
                                                  //|  379, 380, 381), List(382, 383, 384, 385, 386, 387, 388, 389, 390), List(39
                                                  //| 1, 392, 393, 394, 395, 396, 397, 398, 399), List(400, 401, 405, 406), List(
                                                  //| 407, 408, 412, 413), List(414, 415), List(421, 422, 424, 427), List(428, 42
                                                  //| 9, 433, 434), List(488, 489, 490, 491, 492, 493, 494, 495, 496), List(497, 
                                                  //| 498, 499, 500, 501, 502, 503, 504, 505), List(506, 507, 508, 509), List(513
                                                  //| , 514, 515, 516, 517, 518, 519), List(520, 521, 523, 526), List(527, 528, 5
                                                  //| 32, 533), List(534, 535, 536, 537, 538, 539, 540), List(541, 542, 543, 544,
                                                  //|  545, 546, 547, 548, 549), List(550, 551, 552, 553, 554, 555, 556, 557, 558
                                                  //| ), List(559, 560, 561, 562), List(566, 567, 569, 572), List(573, 574, 578, 
                                                  //| 579), List(580, 581, 582, 583, 584, 585, 586), List(587, 588, 589, 590, 591
                                                  //| , 592, 593), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14, 1
                                                  //| 5, 16, 17), List(18, 19, 20, 21, 22, 23, 24, 25, 26), List(27, 28, 29, 30, 
                                                  //| 31, 32, 33), List(34, 35, 36, 37, 38, 39, 40), List(41, 42, 43, 44, 45, 46,
                                                  //|  47), List(48, 49, 50, 51, 52, 53, 54), List(55, 56, 57, 58, 59, 60, 61), L
                                                  //| ist(62, 63, 64, 65, 66, 67, 68), List(207, 208, 209, 210, 211, 212, 213, 21
                                                  //| 4, 215), List(216, 217, 218, 219, 220, 221, 222, 223, 224), List(225, 226, 
                                                  //| 227, 228, 229, 230, 231, 232, 233), List(234, 235, 236, 237, 238, 239, 240)
                                                  //| , List(241, 242, 243, 244, 245, 246, 247), List(248, 249, 250, 251, 252, 25
                                                  //| 3, 254), List(255, 256, 257, 258, 259, 260, 261), List(262, 263, 264, 265, 
                                                  //| 266, 267, 268), List(269, 270, 271, 272, 273, 274, 275), List(329, 330, 331
                                                  //| , 332, 333, 334, 335, 336, 337), List(338, 339, 340, 341, 342, 343, 344, 34
                                                  //| 5, 346), List(347, 348, 349, 350, 351, 352, 353), List(354, 355, 356, 358),
                                                  //|  List(361, 362, 363, 364, 365, 366, 367), List(368, 369, 372, 373), List(37
                                                  //| 5, 376, 379, 380), List(435, 436, 437, 438, 439, 440, 441, 442, 443), List(
                                                  //| 444, 445, 446, 447, 448, 449, 450, 451, 452), List(453, 454, 455, 457), Lis
                                                  //| t(460, 461), List(467, 468, 469, 472), List(474, 475, 479, 480), List(481, 
                                                  //| 482, 485, 486), List(488, 489, 490, 491, 492, 493, 494, 495, 496), List(497
                                                  //| , 498, 499, 500, 501, 502, 503, 504, 505), List(506, 507, 508, 509, 510, 51
                                                  //| 1, 512), List(513, 514, 515, 519), List(520, 521, 522, 525), List(527, 528,
                                                  //|  529, 530, 531, 532, 533), List(534, 535, 539, 540), List(594, 595, 596, 59
                                                  //| 7, 598, 599, 600, 601, 602), List(603, 604, 605, 606, 607, 608, 609, 610, 6
                                                  //| 11), List(612, 613, 614, 618), List(619, 620, 621, 624), List(626, 627, 631
                                                  //| , 632), List(633, 634, 635, 636, 637, 638, 639), List(640, 641, 642, 643, 6
                                                  //| 44, 645, 646), List(859, 860, 861, 862, 863, 864, 865, 866, 867), List(868,
                                                  //|  869, 870, 871, 872, 873, 874, 875, 876), List(877, 878, 879, 881), List(88
                                                  //| 4, 885, 888, 889), List(891, 892, 893, 894, 895, 896, 897), List(898, 899, 
                                                  //| 900, 901, 902, 903, 904), List(905, 906, 909, 910), List(0, 1, 2, 3, 4, 5, 
                                                  //| 6, 7, 8), List(9, 10, 11, 12, 13, 14, 15, 16, 17), List(18, 19, 20, 21, 22,
                                                  //|  23, 24, 25, 26), List(27, 28, 29, 30, 31, 32, 33), List(34, 35, 36, 37, 38
                                                  //| , 39, 40), List(41, 42, 43, 44, 45, 46, 47), List(48, 49, 50, 51, 52, 53, 5
                                                  //| 4), List(55, 56, 57, 58, 59, 60, 61), List(62, 63, 64, 65, 66, 67, 68), Lis
                                                  //| t(138, 139, 140, 141, 142, 143, 144, 145, 146), List(147, 148, 149, 150, 15
                                                  //| 1, 152, 153, 154, 155), List(156, 157, 158, 159, 160, 161, 162, 163, 164), 
                                                  //| List(165, 166, 167, 168, 169, 170, 171), List(172, 173, 174, 175, 176, 177,
                                                  //|  178), List(179, 180, 181, 182, 183, 184, 185), List(186, 187, 188, 189, 19
                                                  //| 0, 191, 192), List(193, 194, 195, 196, 197, 198, 199), List(200, 201, 202, 
                                                  //| 203, 204, 205, 206), List(382, 383, 384, 385, 386, 387, 388, 389, 390), Lis
                                                  //| t(391, 392, 393, 394, 395, 396, 397, 398, 399), List(400, 401, 402, 403, 40
                                                  //| 4, 405, 406), List(407, 408, 409, 410, 411, 412, 413), List(414, 415, 416, 
                                                  //| 417), List(421, 422, 424, 427), List(428, 429, 433, 434), List(435, 436, 43
                                                  //| 7, 438, 439, 440, 441, 442, 443), List(444, 445, 446, 447, 448, 449, 450, 4
                                                  //| 51, 452), List(453, 454, 455, 456, 457, 458, 459), List(460, 461, 462, 466)
                                                  //| , List(467, 468, 469, 472), List(474, 475, 479, 480), List(481, 482, 483, 4
                                                  //| 84, 485, 486, 487), List(488, 489, 490, 491, 492, 493, 494, 495, 496), List
                                                  //| (497, 498, 499, 500, 501, 502, 503, 504, 505), List(506, 507, 508, 509), Li
                                                  //| st(513, 514, 515, 519), List(520, 521), List(527, 528, 532, 533), List(534,
                                                  //|  535, 539, 540), List(541, 542, 543, 544, 545, 546, 547, 548, 549), List(55
                                                  //| 0, 551, 552, 553, 554, 555, 556, 557, 558), List(559, 560, 561, 562), List(
                                                  //| 566, 567, 569, 572), List(573, 574, 578, 579), List(580, 581, 582, 583, 584
                                                  //| , 585, 586), List(587, 588, 589, 590, 591, 592, 593), List(594, 595, 596, 5
                                                  //| 97, 598, 599, 600, 601, 602), List(603, 604, 605, 606, 607, 608, 609, 610, 
                                                  //| 611), List(612, 613, 614, 618), List(619, 620, 621, 624), List(626, 627, 63
                                                  //| 1, 632), List(633, 634, 635, 636, 637, 638, 639), List(640, 641, 642, 643, 
                                                  //| 644, 645, 646), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14
                                                  //| , 15, 16, 17), List(18, 19, 20, 21, 22, 23, 24, 25, 26), List(27, 28, 29, 3
                                                  //| 0, 31, 32, 33), List(34, 35, 36, 37, 38, 39, 40), List(41, 42, 43, 44, 45, 
                                                  //| 46, 47), List(48, 49, 50, 51, 52, 53, 54), List(55, 56, 57, 58, 59, 60, 61)
                                                  //| , List(62, 63, 64, 65, 66, 67, 68), List(69, 70, 71, 72, 73, 74, 75, 76, 77
                                                  //| ), List(78, 79, 80, 81, 82, 83, 84, 85, 86), List(87, 88, 89, 90, 91, 92, 9
                                                  //| 3, 94, 95), List(96, 97, 98, 99, 100, 101, 102), List(103, 104, 105, 106, 1
                                                  //| 07, 108, 109), List(110, 111, 112, 113, 114, 115, 116), List(117, 118, 119,
                                                  //|  120, 121, 122, 123), List(124, 125, 126, 127, 128, 129, 130), List(131, 13
                                                  //| 2, 133, 134, 135, 136, 137), List(382, 383, 384, 385, 386, 387, 388, 389, 3
                                                  //| 90), List(391, 392, 393, 394, 395, 396, 397, 398, 399), List(400, 401, 402,
                                                  //|  403, 404, 405, 406), List(407, 408, 409, 410, 411, 412, 413), List(414, 41
                                                  //| 5, 416, 417), List(421, 422, 424, 427), List(428, 429, 433, 434), List(488,
                                                  //|  489, 490, 491, 492, 493, 494, 495, 496), List(497, 498, 499, 500, 501, 502
                                                  //| , 503, 504, 505), List(506, 507, 508, 509), List(513, 514, 515, 516, 517, 5
                                                  //| 18, 519), List(520, 521, 523, 526), List(527, 528, 532, 533), List(534, 535
                                                  //| , 536, 537, 538, 539, 540), List(541, 542, 543, 544, 545, 546, 547, 548, 54
                                                  //| 9), List(550, 551, 552, 553, 554, 555, 556, 557, 558), List(559, 560, 561, 
                                                  //| 562), List(566, 567, 569, 572), List(573, 574), List(580, 581, 583, 585), L
                                                  //| ist(587, 588, 589, 593), List(647, 648, 649, 650, 651, 652, 653, 654, 655),
                                                  //|  List(656, 657, 658, 659, 660, 661, 662, 663, 664), List(665, 666, 667, 668
                                                  //| ), List(672, 673, 674, 675, 676, 677, 678), List(679, 680, 682, 684), List(
                                                  //| 686, 687, 688, 689, 690, 691, 692), List(693, 694, 695, 699), List(753, 754
                                                  //| , 755, 756, 757, 758, 759, 760, 761), List(762, 763, 764, 765, 766, 767, 76
                                                  //| 8, 769, 770), List(771, 772, 773, 774, 775, 776, 777), List(778, 779, 780, 
                                                  //| 781), List(785, 786, 788, 790), List(792, 793, 794, 798), List(799, 800, 80
                                                  //| 1, 802, 803, 804, 805), List(138, 139, 140, 141, 142, 143, 144, 145, 146), 
                                                  //| List(147, 148, 149, 150, 151, 152, 153, 154, 155), List(156, 157, 158, 159,
                                                  //|  160, 161, 162, 163, 164), List(165, 166, 167, 168, 169, 170, 171), List(17
                                                  //| 2, 173, 174, 175, 176, 177, 178), List(179, 180, 181, 182, 183, 184, 185), 
                                                  //| List(186, 187, 188, 189, 190, 191, 192), List(193, 194, 195, 196, 197, 198,
                                                  //|  199), List(200, 201, 202, 203, 204, 205, 206), List(207, 208, 209, 210, 21
                                                  //| 1, 212, 213, 214, 215), List(216, 217, 218, 219, 220, 221, 222, 223, 224), 
                                                  //| List(225, 226, 227, 228, 229, 230, 231, 232, 233), List(234, 235, 236, 237,
                                                  //|  238, 239, 240), List(241, 242, 243, 244, 245, 246, 247), List(248, 249, 25
                                                  //| 0, 251, 252, 253, 254), List(255, 256, 257, 258, 259, 260, 261), List(262, 
                                                  //| 263, 264, 265, 266, 267, 268), List(269, 270, 271, 272, 273, 274, 275), Lis
                                                  //| t(435, 436, 437, 438, 439, 440, 441, 442, 443), List(444, 445, 446, 447, 44
                                                  //| 8, 449, 450, 451, 452), List(453, 454, 455, 456, 457, 458, 459), List(460, 
                                                  //| 461, 462, 466), List(467, 468, 469, 472), List(474, 475, 479, 480), List(48
                                                  //| 1, 482, 483, 484, 485, 486, 487), List(488, 489, 490, 491, 492, 493, 494, 4
                                                  //| 95, 496), List(497, 498, 499, 500, 501, 502, 503, 504, 505), List(506, 507,
                                                  //|  508, 509, 510, 511, 512), List(513, 514, 515, 519), List(520, 521, 522, 52
                                                  //| 5), List(527, 528, 529, 530, 531, 532, 533), List(534, 535, 539, 540), List
                                                  //| (594, 595, 596, 597, 598, 599, 600, 601, 602), List(603, 604, 605, 606, 607
                                                  //| , 608, 609, 610, 611), List(612, 613, 614, 618), List(619, 620, 621, 624), 
                                                  //| List(626, 627), List(633, 634, 635, 639), List(640, 641, 645, 646), List(64
                                                  //| 7, 648, 649, 650, 651, 652, 653, 654, 655), List(656, 657, 658, 659, 660, 6
                                                  //| 61, 662, 663, 664), List(665, 666, 667, 668, 669, 670, 671), List(672, 673,
                                                  //|  674, 675), List(679, 680, 681, 685), List(686, 687, 691, 692), List(693, 6
                                                  //| 94, 695, 696, 697, 698, 699), List(700, 701, 702, 703, 704, 705, 706, 707, 
                                                  //| 708), List(709, 710, 711, 712, 713, 714, 715, 716, 717), List(718, 719, 720
                                                  //| , 721), List(725, 726, 727, 731), List(732, 733, 737, 738), List(739, 740, 
                                                  //| 741, 742, 743, 744, 745), List(746, 747, 748, 749, 750, 751, 752), List(69,
                                                  //|  70, 71, 72, 73, 74, 75, 76, 77), List(78, 79, 80, 81, 82, 83, 84, 85, 86),
                                                  //|  List(87, 88, 89, 90, 91, 92, 93, 94, 95), List(96, 97, 98, 99, 100, 101, 1
                                                  //| 02), List(103, 104, 105, 106, 107, 108, 109), List(110, 111, 112, 113, 114,
                                                  //|  115, 116), List(117, 118, 119, 120, 121, 122, 123), List(124, 125, 126, 12
                                                  //| 7, 128, 129, 130), List(131, 132, 133, 134, 135, 136, 137), List(207, 208, 
                                                  //| 209, 210, 211, 212, 213, 214, 215), List(216, 217, 218, 219, 220, 221, 222,
                                                  //|  223, 224), List(225, 226, 227, 228, 229, 230, 231, 232, 233), List(234, 23
                                                  //| 5, 236, 237, 238, 239, 240), List(241, 242, 243, 244, 245, 246, 247), List(
                                                  //| 248, 249, 250, 251, 252, 253, 254), List(255, 256, 257, 258, 259, 260, 261)
                                                  //| , List(262, 263, 264, 265, 266, 267, 268), List(269, 270, 271, 272, 273, 27
                                                  //| 4, 275), List(541, 542, 543, 544, 545, 546, 547, 548, 549), List(550, 551, 
                                                  //| 552, 553, 554, 555, 556, 557, 558), List(559, 560, 561, 562, 563, 564, 565)
                                                  //| , List(566, 567, 568, 569, 570, 571, 572), List(573, 574, 575, 576), List(5
                                                  //| 80, 581, 583, 585), List(587, 588, 589, 593), List(594, 595, 596, 597, 598,
                                                  //|  599, 600, 601, 602), List(603, 604, 605, 606, 607, 608, 609, 610, 611), Li
                                                  //| st(612, 613, 614, 615, 616, 617, 618), List(619, 620, 621, 622, 623, 624, 6
                                                  //| 25), List(626, 627, 628, 629), List(633, 634, 635, 639), List(640, 641, 645
                                                  //| , 646), List(647, 648, 649, 650, 651, 652, 653, 654, 655), List(656, 657, 6
                                                  //| 58, 659, 660, 661, 662, 663, 664), List(665, 666, 667, 668), List(672, 673,
                                                  //|  674, 675), List(679, 680), List(686, 687, 691, 692), List(693, 694, 695, 6
                                                  //| 99), List(700, 701, 702, 703, 704, 705, 706, 707, 708), List(709, 710, 711,
                                                  //|  712, 713, 714, 715, 716, 717), List(718, 719, 720, 721), List(725, 726, 72
                                                  //| 7, 731), List(732, 733, 737, 738), List(739, 740, 741, 742, 743, 744, 745),
                                                  //|  List(746, 747, 748, 749, 750, 751, 752), List(753, 754, 755, 756, 757, 758
                                                  //| , 759, 760, 761), List(762, 763, 764, 765, 766, 767, 768, 769, 770), List(7
                                                  //| 71, 772, 773, 774, 775, 776, 777), List(778, 779, 780, 781), List(785, 786,
                                                  //|  788, 790), List(792, 793, 794, 798), List(799, 800, 801, 802, 803, 804, 80
                                                  //| 5), List(69, 70, 71, 72, 73, 74, 75, 76, 77), List(78, 79, 80, 81, 82, 83, 
                                                  //| 84, 85, 86), List(87, 88, 89, 90, 91, 92, 93, 94, 95), List(96, 97, 98, 99,
                                                  //|  100, 101, 102), List(103, 104, 105, 106, 107, 108, 109), List(110, 111, 11
                                                  //| 2, 113, 114, 115, 116), List(117, 118, 119, 120, 121, 122, 123), List(124, 
                                                  //| 125, 126, 127, 128, 129, 130), List(131, 132, 133, 134, 135, 136, 137), Lis
                                                  //| t(138, 139, 140, 141, 142, 143, 144, 145, 146), List(147, 148, 149, 150, 15
                                                  //| 1, 152, 153, 154, 155), List(156, 157, 158, 159, 160, 161, 162, 163, 164), 
                                                  //| List(165, 166, 167, 168, 169, 170, 171), List(172, 173, 174, 175, 176, 177,
                                                  //|  178), List(179, 180, 181, 182, 183, 184, 185), List(186, 187, 188, 189, 19
                                                  //| 0, 191, 192), List(193, 194, 195, 196, 197, 198, 199), List(200, 201, 202, 
                                                  //| 203, 204, 205, 206), List(594, 595, 596, 597, 598, 599, 600, 601, 602), Lis
                                                  //| t(603, 604, 605, 606, 607, 608, 609, 610, 611), List(612, 613, 614, 615, 61
                                                  //| 6, 617, 618), List(619, 620, 621, 622, 623, 624, 625), List(626, 627, 628, 
                                                  //| 629), List(633, 634, 635, 639), List(640, 641, 645, 646), List(647, 648, 64
                                                  //| 9, 650, 651, 652, 653, 654, 655), List(656, 657, 658, 659, 660, 661, 662, 6
                                                  //| 63, 664), List(665, 666, 667, 668, 669, 670, 671), List(672, 673, 674, 675)
                                                  //| , List(679, 680, 681, 685), List(686, 687, 691, 692), List(693, 694, 695, 6
                                                  //| 96, 697, 698, 699), List(700, 701, 702, 703, 704, 705, 706, 707, 708), List
                                                  //| (709, 710, 711, 712, 713, 714, 715, 716, 717), List(718, 719, 720, 721), Li
                                                  //| st(725, 726, 727, 731), List(732, 733), List(739, 740, 741, 743), List(746,
                                                  //|  747, 748, 749), List(806, 807, 808, 809, 810, 811, 812, 813, 814), List(81
                                                  //| 5, 816, 817, 818, 819, 820, 821, 822, 823), List(824, 825, 826, 827, 828, 8
                                                  //| 29, 830), List(831, 832, 833, 834), List(838, 839, 840, 841, 842, 843, 844)
                                                  //| , List(845, 846, 847, 849), List(852, 853, 854, 855), List(859, 860, 861, 8
                                                  //| 62, 863, 864, 865, 866, 867), List(868, 869, 870, 871, 872, 873, 874, 875, 
                                                  //| 876), List(877, 878, 879, 880, 881, 882, 883), List(884, 885, 886, 887, 888
                                                  //| , 889, 890), List(891, 892, 893, 894), List(898, 899, 900, 902), List(905, 
                                                  //| 906, 907, 908), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14
                                                  //| , 15, 16, 17), List(18, 19, 20, 21, 22, 23, 24, 25, 26), List(27, 28, 29, 3
                                                  //| 0, 31, 32, 33), List(34, 35, 36, 37, 38, 39, 40), List(41, 42, 43, 44, 45, 
                                                  //| 46, 47), List(48, 49, 50, 51, 52, 53, 54), List(55, 56, 57, 58, 59, 60, 61)
                                                  //| , List(62, 63, 64, 65, 66, 67, 68), List(207, 208, 209, 210, 211, 212, 213,
                                                  //|  214, 215), List(216, 217, 218, 219, 220, 221, 222, 223, 224), List(225, 22
                                                  //| 6, 227, 228, 229, 230, 231, 232, 233), List(234, 235, 236, 237, 238, 239, 2
                                                  //| 40), List(241, 242, 243, 244, 245, 246, 247), List(248, 249, 250, 251, 252,
                                                  //|  253, 254), List(255, 256, 257, 258, 259, 260, 261), List(262, 263, 264, 26
                                                  //| 5, 266, 267, 268), List(269, 270, 271, 272, 273, 274, 275), List(276, 277, 
                                                  //| 278, 279, 280, 281, 282, 283, 284), List(285, 286, 287, 288, 289, 290, 291,
                                                  //|  292, 293), List(294, 295, 297, 298), List(301, 302, 303, 304, 305, 306, 30
                                                  //| 7), List(308, 309, 310, 311, 312, 313, 314), List(315, 316, 318, 319), List
                                                  //| (322, 323, 325, 328), List(541, 542, 543, 544, 545, 546, 547, 548, 549), Li
                                                  //| st(550, 551, 552, 553, 554, 555, 556, 557, 558), List(559, 560, 561, 562, 5
                                                  //| 63, 564, 565), List(566, 567, 568, 569, 570, 571, 572), List(573, 574, 575,
                                                  //|  576), List(580, 581, 583, 585), List(587, 588, 589, 593), List(647, 648, 6
                                                  //| 49, 650, 651, 652, 653, 654, 655), List(656, 657, 658, 659, 660, 661, 662, 
                                                  //| 663, 664), List(665, 666, 667, 668), List(672, 673, 674, 675, 676, 677, 678
                                                  //| ), List(679, 680, 682, 684), List(686, 687, 688, 689, 690, 691, 692), List(
                                                  //| 693, 694, 695, 699), List(753, 754, 755, 756, 757, 758, 759, 760, 761), Lis
                                                  //| t(762, 763, 764, 765, 766, 767, 768, 769, 770), List(771, 772, 774, 775), L
                                                  //| ist(778, 779, 780, 781), List(785, 786, 788, 790), List(792, 793), List(799
                                                  //| , 800, 802, 805), List(806, 807, 808, 809, 810, 811, 812, 813, 814), List(8
                                                  //| 15, 816, 817, 818, 819, 820, 821, 822, 823), List(824, 825, 827, 828), List
                                                  //| (831, 832, 833, 834, 835, 836, 837), List(838, 839, 841, 842), List(845, 84
                                                  //| 6, 848, 851), List(852, 853, 854, 855, 856, 857, 858), List(0, 1, 2, 3, 4, 
                                                  //| 5, 6, 7, 8), List(9, 10, 11, 12, 13, 14, 15, 16, 17), List(18, 19, 20, 21, 
                                                  //| 22, 23, 24, 25, 26), List(27, 28, 29, 30, 31, 32, 33), List(34, 35, 36, 37,
                                                  //|  38, 39, 40), List(41, 42, 43, 44, 45, 46, 47), List(48, 49, 50, 51, 52, 53
                                                  //| , 54), List(55, 56, 57, 58, 59, 60, 61), List(62, 63, 64, 65, 66, 67, 68), 
                                                  //| List(138, 139, 140, 141, 142, 143, 144, 145, 146), List(147, 148, 149, 150,
                                                  //|  151, 152, 153, 154, 155), List(156, 157, 158, 159, 160, 161, 162, 163, 164
                                                  //| ), List(165, 166, 167, 168, 169, 170, 171), List(172, 173, 174, 175, 176, 1
                                                  //| 77, 178), List(179, 180, 181, 182, 183, 184, 185), List(186, 187, 188, 189,
                                                  //|  190, 191, 192), List(193, 194, 195, 196, 197, 198, 199), List(200, 201, 20
                                                  //| 2, 203, 204, 205, 206), List(276, 277, 278, 279, 280, 281, 282, 283, 284), 
                                                  //| List(285, 286, 287, 288, 289, 290, 291, 292, 293), List(294, 295, 297, 298)
                                                  //| , List(301, 302, 303, 304, 305, 306, 307), List(308, 309, 310, 311, 312, 31
                                                  //| 3, 314), List(315, 316, 318, 319), List(322, 323, 325, 328), List(700, 701,
                                                  //|  702, 703, 704, 705, 706, 707, 708), List(709, 710, 711, 712, 713, 714, 715
                                                  //| , 716, 717), List(718, 719, 720, 721, 722, 723, 724), List(725, 726, 727, 7
                                                  //| 28, 729, 730, 731), List(732, 733, 734, 735), List(739, 740, 741, 743), Lis
                                                  //| t(746, 747, 748, 749), List(753, 754, 755, 756, 757, 758, 759, 760, 761), L
                                                  //| ist(762, 763, 764, 765, 766, 767, 768, 769, 770), List(771, 772, 774, 775),
                                                  //|  List(778, 779, 780, 781, 782, 783, 784), List(785, 786, 787, 788, 789, 790
                                                  //| , 791), List(792, 793, 795, 796), List(799, 800, 802, 805), List(806, 807, 
                                                  //| 808, 809, 810, 811, 812, 813, 814), List(815, 816, 817, 818, 819, 820, 821,
                                                  //|  822, 823), List(824, 825, 827, 828), List(831, 832, 833, 834), List(838, 8
                                                  //| 39, 841, 842), List(845, 846), List(852, 853, 854, 855), List(859, 860, 861
                                                  //| , 862, 863, 864, 865, 866, 867), List(868, 869, 870, 871, 872, 873, 874, 87
                                                  //| 5, 876), List(877, 878, 879, 880, 881, 882, 883), List(884, 885, 886, 887, 
                                                  //| 888, 889, 890), List(891, 892, 893, 894), List(898, 899, 900, 902), List(90
                                                  //| 5, 906, 907, 908), List(0, 1, 2, 3, 4, 5, 6, 7, 8), List(9, 10, 11, 12, 13,
                                                  //|  14, 15, 16, 17), List(18, 19, 20, 21, 22, 23, 24, 25, 26), List(27, 28, 29
                                                  //| , 30, 31, 32, 33), List(34, 35, 36, 37, 38, 39, 40), List(41, 42, 43, 44, 4
                                                  //| 5, 46, 47), List(48, 49, 50, 51, 52, 53, 54), List(55, 56, 57, 58, 59, 60, 
                                                  //| 61), List(62, 63, 64, 65, 66, 67, 68), List(69, 70, 71, 72, 73, 74, 75, 76,
                                                  //|  77), List(78, 79, 80, 81, 82, 83, 84, 85, 86), List(87, 88, 89, 90, 91, 92
                                                  //| , 93, 94, 95), List(96, 97, 98, 99, 100, 101, 102), List(103, 104, 105, 106
                                                  //| , 107, 108, 109), List(110, 111, 112, 113, 114, 115, 116), List(117, 118, 1
                                                  //| 19, 120, 121, 122, 123), List(124, 125, 126, 127, 128, 129, 130), List(131,
                                                  //|  132, 133, 134, 135, 136, 137), List(329, 330, 331, 332, 333, 334, 335, 336
                                                  //| , 337), List(338, 339, 340, 341, 342, 343, 344, 345, 346), List(347, 348, 3
                                                  //| 49, 350, 351, 352, 353), List(354, 355, 356, 358), List(361, 362, 363, 364,
                                                  //|  365, 366, 367), List(368, 369, 372, 373), List(375, 376, 379, 380), List(4
                                                  //| 35, 436, 437, 438, 439, 440, 441, 442, 443), List(444, 445, 446, 447, 448, 
                                                  //| 449, 450, 451, 452), List(453, 454, 455, 457), List(460, 461, 464, 465), Li
                                                  //| st(467, 468, 469, 470, 471, 472, 473), List(474, 475, 476, 477, 478, 479, 4
                                                  //| 80), List(481, 482, 485, 486), List(700, 701, 702, 703, 704, 705, 706, 707,
                                                  //|  708), List(709, 710, 711, 712, 713, 714, 715, 716, 717), List(718, 719, 72
                                                  //| 0, 721, 722, 723, 724), List(725, 726, 727, 728, 729, 730, 731), List(732, 
                                                  //| 733, 734, 735), List(739, 740, 741, 743), List(746, 747, 748, 749), List(80
                                                  //| 6, 807, 808, 809, 810, 811, 812, 813, 814), List(815, 816, 817, 818, 819, 8
                                                  //| 20, 821, 822, 823), List(824, 825, 826, 827, 828, 829, 830), List(831, 832,
                                                  //|  833, 834), List(838, 839, 840, 841, 842, 843, 844), List(845, 846, 847, 84
                                                  //| 9), List(852, 853, 854, 855), List(859, 860, 861, 862, 863, 864, 865, 866, 
                                                  //| 867), List(868, 869, 870, 871, 872, 873, 874, 875, 876), List(877, 878, 879
                                                  //| , 881), List(884, 885, 888, 889), List(891, 892, 893, 894), List(898, 899, 
                                                  //| 900, 902), List(905, 906))

    three_States_Map.size                         //> res10: Int = 912
    three_States_Map.flatten.size                 //> res11: Int = 6300
    // So three_States_Map should be the transition matrix we need to generate the next map
    // with a transition matrix T, multiplying by T^2 should advance it 2 positions.
    // multiplying by T^4 should advance it by 4...
    // We can continue to square the matrix m times, resulting in a T^(2^m). So I think we only have to do 24 or 256 squarings.

    // OK, we just need a matrix square call and a matrix times matrix.
    // row times matrix
    // matrix times row
    type bigNumberType = BigInt // Long results in negative result! Must use BigInt

    type Matrix = Array[Array[bigNumberType]]

    val T: Matrix = three_States_Map.map { list => (0 until three_States.size).toArray.map(i =>
     if (list.contains(i)) BigInt("1") /*.asInstanceOf[bigNumberType] */ else BigInt("0") /*.asInstanceOf[bigNumberType] */ ) }
                                                  //> T  : ponder.Matrix = Array(Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array
                                                  //| (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1,
                                                  //|  1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Arra
                                                  //| y(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1
                                                  //| , 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 
                                                  //| 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1,
                                                  //|  1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Arr
                                                  //| ay(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1,
                                                  //|  1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1
                                                  //| , 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 
                                                  //| 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Ar
                                                  //| ray(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), A
                                                  //| rray(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1,
                                                  //|  1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0
                                                  //| , 0, 0, 0, 0, 0, 0), Array(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //| Output exceeds cutoff limit.

    val N = BigInt("3141592653")//.asInstanceOf[bigNumberType]
                                                  //> N  : scala.math.BigInt = 3141592653
   
    def rowTimesMatrix(r: Array[bigNumberType], m: Matrix): Array[bigNumberType] = {
     for ( column <- m.transpose )
      // bigInts don't need to have their mod taken after every multiply. Longs will overflow at this multiplication.
      // But bigInts tend to take so much memory that it crashes, so let's do the %N after each multiplication to keep them from growing so large
       yield  (r.zip(column).map(x => x._1 * x._2 % N).sum ) % N
    }                                             //> rowTimesMatrix: (r: Array[ponder.bigNumberType], m: ponder.Matrix)Array[pon
                                                  //| der.bigNumberType]

    
    var multCounter = 0                           //> multCounter  : Int = 0
    def matrixTimesMatrix(m1: Matrix, m2: Matrix): Matrix = {
     println("multiplying two matrices... " + multCounter)
     multCounter = multCounter + 1
     // Take out the .par to run this on the command line >=scala-2.13
     (for (row <- m1.par)
      yield rowTimesMatrix(row, m1)
      ).toArray
     }                                            //> matrixTimesMatrix: (m1: ponder.Matrix, m2: ponder.Matrix)ponder.Matrix

    def matrixSquared(m: Matrix): Matrix = {
      /*println("squaring a matrix " + squareCounter)
      squareCounter = squareCounter + 1*/
      matrixTimesMatrix(m, m)
    }                                             //> matrixSquared: (m: ponder.Matrix)ponder.Matrix


    def matrixTimesColumnOfOnes(m: Matrix): Array[bigNumberType] = {
      m.map(_.sum)
    }                                             //> matrixTimesColumnOfOnes: (m: ponder.Matrix)Array[ponder.bigNumberType]

    // Starting with a row of ones because we get one entry per state when we get to the 3-state for the first time.
    // Final multiply of column ones is to add up how many total routines there are. We have a row Array breaking down
    // all the different routines, so we just add them up.
    // Equivalent to adding up all the entries
    def rowOfOnesTimesMatrixTimesColumnOfOnes(m: Matrix): bigNumberType = {
      m.map(_.sum).sum % N
    }                                             //> rowOfOnesTimesMatrixTimesColumnOfOnes: (m: ponder.Matrix)ponder.bigNumberT
                                                  //| ype

    // 2^24 - 3
    // 10000 with 24 zeroes ( let's check 10 is 2^1 with 1 zero).
    // -  11  3 in binary
    //  1101  so 22 ones followed by 01.
    // So from right to left, that's T^2^0 * T^2^2 * T^2^3 * T^2^4... * T^2^23
    println("here1")                              //> here1
    // ScanLeft creates 1 more entry than you think it will
    // (0 to 1).scanLeft will create
    // T, T^2, T^4.
    // i.e. it prepends the "zero" value and does the operation once for each element in the array.
// for {
 /*
  val allMs = (0 to 1).scanLeft(T)((m, i) => matrixSquared(m))
   
    val smallSolutins = for{ nMinus3 <- (4 to 8).map(x=>  BigInt(x) - 3)}  /*BigInt(2).pow(3) -3 */
    yield {
     val nBinaryStringReversed = nMinus3.toString(2).reverse
     println("n " + (nMinus3 + 3) + " nMinus3 " + nMinus3 + " binary reversed " + nBinaryStringReversed)
     val finalRow = nBinaryStringReversed.toCharArray().zip(allMs).foldLeft( Array.fill(three_States.size)(BigInt("1")) )  { case (row,(bit, matrix)) =>
      bit match {
       case '1' => rowTimesMatrix(row, matrix)
       case '0' => row
      }
     }
     println("  (nMinus3 + 3, finalRow.sum)  " +   (nMinus3 + 3, finalRow.sum))
   (nMinus3 + 3, finalRow.sum)
   }
   */
  // Vector((4,6300), (5,45876), (6,331884), (7,2401092)
   
   // Starting at 6:40 PM August 21
   val bonusSolution = for { nMinus3 <- List(24/*, 256*/).map( power => BigInt(2).pow(power) -3 ) }
   yield {
    val nBinaryStringReversed = nMinus3.toString(2).reverse
    val s = nBinaryStringReversed.toCharArray().foldLeft( Array.fill(three_States.size)(BigInt("1")), T )  { case ((row, matrix), bit) =>
     println(" intermediate answer with bit " + bit + " " + row.sum)
     bit match {
      case '1' => (rowTimesMatrix(row, matrix), matrixSquared(matrix))
      case '0' => (row,                         matrixSquared(matrix))
     }
    }
    println("Final answer " + (s._1.sum % N))
   }                                              //>  intermediate answer with bit 1 912
                                                  //| multiplying two matrices... 0
                                                  //|  intermediate answer with bit 0 6300
                                                  //| multiplying two matrices... 1
                                                  //|  intermediate answer with bit 1 6300
                                                  //| multiplying two matrices... 2
                                                  //|  intermediate answer with bit 1 17342172
                                                  //| multiplying two matrices... 3
                                                  //|  intermediate answer with bit 1 1497347962380
                                                  //| multiplying two matrices... 4
                                                  //|  intermediate answer with bit 1 1659591287316
                                                  //| multiplying two matrices... 5
                                                  //|  intermediate answer with bit 1 1640529298764
                                                  //| multiplying two matrices... 6
                                                  //|  intermediate answer with bit 1 1413059551092
                                                  //| multiplying two matrices... 7
                                                  //|  intermediate answer with bit 1 1385828709516
                                                  //| multiplying two matrices... 8
                                                  //|  intermediate answer with bit 1 1704457052892
                                                  //| multiplying two matrices... 9
                                                  //|  intermediate answer with bit 1 1453986940644
                                                  //| multiplying two matrices... 10
                                                  //|  intermediate answer with bit 1 1505845195200
                                                  //| multiplying two matrices... 11
                                                  //|  intermediate answer with bit 1 1529422293456
                                                  //| multiplying two matrices... 12
                                                  //|  intermediate answer with bit 1 1137445294428
                                                  //| multiplying two matrices... 13
                                                  //|  intermediate answer with bit 1 1558980220752
                                                  //| multiplying two matrices... 14
                                                  //|  intermediate answer with bit 1 1626204820032
                                                  //| multiplying two matrices... 15
                                                  //|  intermediate answer with bit 1 1488034546416
                                                  //| multiplying two matrices... 16
                                                  //|  intermediate answer with bit 1 1472265853812
                                                  //| multiplying two matrices... 17
                                                  //|  intermediate answer with bit 1 1682739589464
                                                  //| multiplying two matrices... 18
                                                  //|  intermediate answer with bit 1 1110014725320
                                                  //| multiplying two matrices... 19
                                                  //|  intermediate answer with bit 1 1567544269032
                                                  //| multiplying two matrices... 20
                                                  //|  intermediate answer with bit 1 1515283010568
                                                  //| multiplying two matrices... 21
                                                  //|  intermediate answer with bit 1 1057043085156
                                                  //| multiplying two matrices... 22
                                                  //|  intermediate answer with bit 1 425183908176
                                                  //| multiplying two matrices... 23
                                                  //| Final answer 2524614399
                                                  //| bonusSolution  : List[Unit] = List(())
   /*
   3 intermediate answer with bit 1 912
multiplying two matrices... 0
4 intermediate answer with bit 0 6300
multiplying two matrices... 1
 intermediate answer with bit 1 6300
multiplying two matrices... 2
8=2^3 intermediate answer with bit 1 17342172
multiplying two matrices... 3
4 intermediate answer with bit 1 1497347962380
multiplying two matrices... 4
5 intermediate answer with bit 1 1659591287316
multiplying two matrices... 5
6 intermediate answer with bit 1 1640529298764
multiplying two matrices... 6
7 intermediate answer with bit 1 1413059551092
multiplying two matrices... 7
8 intermediate answer with bit 1 1385828709516
multiplying two matrices... 8
9 intermediate answer with bit 1 1704457052892
multiplying two matrices... 9
10 intermediate answer with bit 1 1453986940644
multiplying two matrices... 10
11 intermediate answer with bit 1 1505845195200
multiplying two matrices... 11
12 intermediate answer with bit 1 1529422293456
multiplying two matrices... 12
 intermediate answer with bit 1 1137445294428
multiplying two matrices... 13
 intermediate answer with bit 1 1558980220752
multiplying two matrices... 14
 intermediate answer with bit 1 1626204820032
multiplying two matrices... 15
 intermediate answer with bit 1 1488034546416
multiplying two matrices... 16
 intermediate answer with bit 1 1472265853812
multiplying two matrices... 17
 intermediate answer with bit 1 1682739589464
multiplying two matrices... 18
 intermediate answer with bit 1 1110014725320
multiplying two matrices... 19
 intermediate answer with bit 1 1567544269032
multiplying two matrices... 20
 intermediate answer with bit 1 1515283010568
multiplying two matrices... 21
 intermediate answer with bit 1 1057043085156
multiplying two matrices... 22
 intermediate answer with bit 1 425183908176
multiplying two matrices... 23
 intermediate answer with bit 1 1425666086208
So this is the answer to 2^24 1425666086208

multiplying two matrices... 24
 intermediate answer with bit 1 1077420193272
multiplying two matrices... 25
 intermediate answer with bit 1 1705700159352
multiplying two matrices... 26
 intermediate answer with bit 1 1425524822172
multiplying two matrices... 27
 intermediate answer with bit 1 685888759152
multiplying two matrices... 28
 intermediate answer with bit 1 1425854347416
multiplying two matrices... 29
 intermediate answer with bit 1 1281204660528
multiplying two matrices... 30
 intermediate answer with bit 1 1024484296824
multiplying two matrices... 31
 intermediate answer with bit 1 1982334425868
multiplying two matrices... 32
 intermediate answer with bit 1 799362623160
multiplying two matrices... 33
 intermediate answer with bit 1 1094943278016
multiplying two matrices... 34
 intermediate answer with bit 1 1416233081628
multiplying two matrices... 35
 intermediate answer with bit 1 1459187869680
multiplying two matrices... 36
 intermediate answer with bit 1 1575882921096
multiplying two matrices... 37
 intermediate answer with bit 1 1156687663284
multiplying two matrices... 38
 intermediate answer with bit 1 2058285965688
multiplying two matrices... 39
 intermediate answer with bit 1 1326690356328
multiplying two matrices... 40
 intermediate answer with bit 1 1326156619140
multiplying two matrices... 41
 intermediate answer with bit 1 1595994146136
multiplying two matrices... 42
 intermediate answer with bit 1 2110108106916
multiplying two matrices... 43
 intermediate answer with bit 1 1857516247080
multiplying two matrices... 44
 intermediate answer with bit 1 1445434178184
multiplying two matrices... 45
 intermediate answer with bit 1 1375853804964
multiplying two matrices... 46
 intermediate answer with bit 1 2137386767940
multiplying two matrices... 47
 intermediate answer with bit 1 1571254984368
multiplying two matrices... 48
 intermediate answer with bit 1 1062543293892
multiplying two matrices... 49
 intermediate answer with bit 1 1521278031744
multiplying two matrices... 50
 intermediate answer with bit 1 1519009364412
multiplying two matrices... 51
 intermediate answer with bit 1 1887501904092
multiplying two matrices... 52
 intermediate answer with bit 1 1255057713036
multiplying two matrices... 53
 intermediate answer with bit 1 1804373134488
multiplying two matrices... 54
 intermediate answer with bit 1 949896306540
multiplying two matrices... 55
 intermediate answer with bit 1 1255002726492
multiplying two matrices... 56
 intermediate answer with bit 1 2120130754452
multiplying two matrices... 57
 intermediate answer with bit 1 1235079110592
multiplying two matrices... 58
 intermediate answer with bit 1 881878865832
multiplying two matrices... 59
 intermediate answer with bit 1 1065097966212
multiplying two matrices... 60
 intermediate answer with bit 1 1792979284464
multiplying two matrices... 61
 intermediate answer with bit 1 1710363847032
multiplying two matrices... 62
 intermediate answer with bit 1 521922635208
multiplying two matrices... 63
 intermediate answer with bit 1 1121224996620
multiplying two matrices... 64
 intermediate answer with bit 1 959772178800
multiplying two matrices... 65
 intermediate answer with bit 1 1570171935456
multiplying two matrices... 66
 intermediate answer with bit 1 1629316784952
multiplying two matrices... 67
 intermediate answer with bit 1 1074910418604
multiplying two matrices... 68
 intermediate answer with bit 1 2025782733924
multiplying two matrices... 69
 intermediate answer with bit 1 705347996904
multiplying two matrices... 70
 intermediate answer with bit 1 1340120590200
multiplying two matrices... 71
 intermediate answer with bit 1 1582305921216
multiplying two matrices... 72
 intermediate answer with bit 1 973372238892
multiplying two matrices... 73
 intermediate answer with bit 1 1565965381788
multiplying two matrices... 74
 intermediate answer with bit 1 1181694050460
multiplying two matrices... 75
 intermediate answer with bit 1 1036754008272
multiplying two matrices... 76
 intermediate answer with bit 1 955873077696
multiplying two matrices... 77
 intermediate answer with bit 1 1204542776808
multiplying two matrices... 78
 intermediate answer with bit 1 1520032613760
multiplying two matrices... 79
 intermediate answer with bit 1 1126880854596
multiplying two matrices... 80
 intermediate answer with bit 1 1721840270292
multiplying two matrices... 81
 intermediate answer with bit 1 949100687352
multiplying two matrices... 82
 intermediate answer with bit 1 882232082928
multiplying two matrices... 83
 intermediate answer with bit 1 997745822640
multiplying two matrices... 84
 intermediate answer with bit 1 1739533349628
multiplying two matrices... 85
 intermediate answer with bit 1 1794919526388
multiplying two matrices... 86
 intermediate answer with bit 1 1549461947976
multiplying two matrices... 87
 intermediate answer with bit 1 888023460924
multiplying two matrices... 88
 intermediate answer with bit 1 1266288045012
multiplying two matrices... 89
 intermediate answer with bit 1 1486859956956
multiplying two matrices... 90
 intermediate answer with bit 1 1472720708412
multiplying two matrices... 91
 intermediate answer with bit 1 1214237628324
multiplying two matrices... 92
 intermediate answer with bit 1 1989317853576
multiplying two matrices... 93
 intermediate answer with bit 1 1967504616432
multiplying two matrices... 94
 intermediate answer with bit 1 1555197248340
multiplying two matrices... 95
 intermediate answer with bit 1 918919529784
multiplying two matrices... 96
 intermediate answer with bit 1 968390717004
multiplying two matrices... 97
 intermediate answer with bit 1 1874449885212
multiplying two matrices... 98
 intermediate answer with bit 1 2119737573540
multiplying two matrices... 99
 intermediate answer with bit 1 1719117805176
multiplying two matrices... 100
 intermediate answer with bit 1 1101310775352
multiplying two matrices... 101
 intermediate answer with bit 1 1748804077980
multiplying two matrices... 102
 intermediate answer with bit 1 1750510976904
multiplying two matrices... 103
 intermediate answer with bit 1 1276099381224
multiplying two matrices... 104
 intermediate answer with bit 1 1794195692496
multiplying two matrices... 105
 intermediate answer with bit 1 1903282906212
multiplying two matrices... 106
 intermediate answer with bit 1 1935967683132
multiplying two matrices... 107
 intermediate answer with bit 1 1940223216132
multiplying two matrices... 108
 intermediate answer with bit 1 1080877660056
multiplying two matrices... 109
 intermediate answer with bit 1 2097320768424
multiplying two matrices... 110
 intermediate answer with bit 1 1511915489460
multiplying two matrices... 111
 intermediate answer with bit 1 1028426702832
multiplying two matrices... 112
 intermediate answer with bit 1 1413482656392
multiplying two matrices... 113
 intermediate answer with bit 1 1648115080128
multiplying two matrices... 114
 intermediate answer with bit 1 2009211282900
multiplying two matrices... 115
 intermediate answer with bit 1 1498888962864
multiplying two matrices... 116
 intermediate answer with bit 1 940529192976
multiplying two matrices... 117
 intermediate answer with bit 1 1200399732216
multiplying two matrices... 118
 intermediate answer with bit 1 1032734235636
multiplying two matrices... 119
 intermediate answer with bit 1 1216983843324
multiplying two matrices... 120
 intermediate answer with bit 1 1343136382128
multiplying two matrices... 121
 intermediate answer with bit 1 559421226504
multiplying two matrices... 122
 intermediate answer with bit 1 1521725181552
multiplying two matrices... 123
 intermediate answer with bit 1 1659805624152
multiplying two matrices... 124
 intermediate answer with bit 1 1863530749332
multiplying two matrices... 125
 intermediate answer with bit 1 1232361218736
multiplying two matrices... 126
 intermediate answer with bit 1 1928595191976
multiplying two matrices... 127
 intermediate answer with bit 1 1567076347872
multiplying two matrices... 128
 intermediate answer with bit 1 2231283132252
multiplying two matrices... 129
 intermediate answer with bit 1 787129922808
multiplying two matrices... 130
 intermediate answer with bit 1 952158935628
multiplying two matrices... 131
 intermediate answer with bit 1 2105022260880
multiplying two matrices... 132
 intermediate answer with bit 1 893966167332
multiplying two matrices... 133
 intermediate answer with bit 1 969467567436
multiplying two matrices... 134
 intermediate answer with bit 1 659819519892
multiplying two matrices... 135
 intermediate answer with bit 1 1825631387496
multiplying two matrices... 136
 intermediate answer with bit 1 1073044594440
multiplying two matrices... 137
 intermediate answer with bit 1 873156366972
multiplying two matrices... 138
 intermediate answer with bit 1 1487096133228
multiplying two matrices... 139
 intermediate answer with bit 1 927711744228
multiplying two matrices... 140
 intermediate answer with bit 1 2176285001400
multiplying two matrices... 141
 intermediate answer with bit 1 1210450227444
multiplying two matrices... 142
 intermediate answer with bit 1 951623138340
multiplying two matrices... 143
 intermediate answer with bit 1 1101327783444
multiplying two matrices... 144
 intermediate answer with bit 1 2072159586432
multiplying two matrices... 145
 intermediate answer with bit 1 1563684622056
multiplying two matrices... 146
 intermediate answer with bit 1 1224612118908
multiplying two matrices... 147
 intermediate answer with bit 1 1606200598224
multiplying two matrices... 148
 intermediate answer with bit 1 1747004009400
multiplying two matrices... 149
 intermediate answer with bit 1 2362325697252
multiplying two matrices... 150
 intermediate answer with bit 1 1565549195256
multiplying two matrices... 151
 intermediate answer with bit 1 1628418975408
multiplying two matrices... 152
 intermediate answer with bit 1 1463896993536
multiplying two matrices... 153
 intermediate answer with bit 1 1754025312708
multiplying two matrices... 154
 intermediate answer with bit 1 1986008104152
multiplying two matrices... 155
 intermediate answer with bit 1 984389168484
multiplying two matrices... 156
 intermediate answer with bit 1 836456618700
multiplying two matrices... 157
 intermediate answer with bit 1 1083445035516
multiplying two matrices... 158
 intermediate answer with bit 1 1071746480988
multiplying two matrices... 159
 intermediate answer with bit 1 1400447604384
multiplying two matrices... 160
 intermediate answer with bit 1 1275178578948
multiplying two matrices... 161
 intermediate answer with bit 1 1365626308644
multiplying two matrices... 162
 intermediate answer with bit 1 1605841269588
multiplying two matrices... 163
 intermediate answer with bit 1 1970168816376
multiplying two matrices... 164
 intermediate answer with bit 1 1776342153060
multiplying two matrices... 165
 intermediate answer with bit 1 1898795728656
multiplying two matrices... 166
 intermediate answer with bit 1 1128965273208
multiplying two matrices... 167
 intermediate answer with bit 1 1495750345920
multiplying two matrices... 168
 intermediate answer with bit 1 1345768792668
multiplying two matrices... 169
 intermediate answer with bit 1 1645470558504
multiplying two matrices... 170
 intermediate answer with bit 1 1031006548764
multiplying two matrices... 171
 intermediate answer with bit 1 1328826923928
multiplying two matrices... 172
 intermediate answer with bit 1 2441190245472
multiplying two matrices... 173
 intermediate answer with bit 1 1752546519936
multiplying two matrices... 174
 intermediate answer with bit 1 1044126530568
multiplying two matrices... 175
 intermediate answer with bit 1 1918458110484
multiplying two matrices... 176
 intermediate answer with bit 1 1395081866952
multiplying two matrices... 177
 intermediate answer with bit 1 1314470343780
multiplying two matrices... 178
 intermediate answer with bit 1 1290899112660
multiplying two matrices... 179
 intermediate answer with bit 1 1492946676324
multiplying two matrices... 180
 intermediate answer with bit 1 1225030578084
multiplying two matrices... 181
 intermediate answer with bit 1 1207063424016
multiplying two matrices... 182
 intermediate answer with bit 1 730373813532
multiplying two matrices... 183
 intermediate answer with bit 1 1172274949548
multiplying two matrices... 184
 intermediate answer with bit 1 1993335368148
multiplying two matrices... 185
 intermediate answer with bit 1 1698782404788
multiplying two matrices... 186
 intermediate answer with bit 1 1441637197524
multiplying two matrices... 187
 intermediate answer with bit 1 1265685429960
multiplying two matrices... 188
 intermediate answer with bit 1 838077102720
multiplying two matrices... 189
 intermediate answer with bit 1 1955449235304
multiplying two matrices... 190
 intermediate answer with bit 1 989490143376
multiplying two matrices... 191
 intermediate answer with bit 1 1510575616440
multiplying two matrices... 192
 intermediate answer with bit 1 1485418907412
multiplying two matrices... 193
 intermediate answer with bit 1 1462314810744
multiplying two matrices... 194
 intermediate answer with bit 1 2403469136412
multiplying two matrices... 195
 intermediate answer with bit 1 1674673708536
multiplying two matrices... 196
 intermediate answer with bit 1 1282505780412
multiplying two matrices... 197
 intermediate answer with bit 1 2260962382680
multiplying two matrices... 198
 intermediate answer with bit 1 1409719509252
multiplying two matrices... 199
 intermediate answer with bit 1 1595698925292
multiplying two matrices... 200
 intermediate answer with bit 1 1759730215680
multiplying two matrices... 201
 intermediate answer with bit 1 1385088940908
multiplying two matrices... 202
 intermediate answer with bit 1 1459294086024
multiplying two matrices... 203
 intermediate answer with bit 1 642886516404
multiplying two matrices... 204
 intermediate answer with bit 1 1410798075084
multiplying two matrices... 205
 intermediate answer with bit 1 1507257252756
multiplying two matrices... 206
 intermediate answer with bit 1 1607925910356
multiplying two matrices... 207
 intermediate answer with bit 1 1537079604588
multiplying two matrices... 208
 intermediate answer with bit 1 1715489352504
multiplying two matrices... 209
 intermediate answer with bit 1 1667028497544
multiplying two matrices... 210
 intermediate answer with bit 1 1469043680364
multiplying two matrices... 211
 intermediate answer with bit 1 1478833650828
multiplying two matrices... 212
 intermediate answer with bit 1 1737660376656
multiplying two matrices... 213
 intermediate answer with bit 1 2190934744128
multiplying two matrices... 214
 intermediate answer with bit 1 1795742318160
multiplying two matrices... 215
 intermediate answer with bit 1 1419203267028
multiplying two matrices... 216
 intermediate answer with bit 1 1358123078016
multiplying two matrices... 217
 intermediate answer with bit 1 1504018518552
multiplying two matrices... 218
 intermediate answer with bit 1 1562685059700
multiplying two matrices... 219
 intermediate answer with bit 1 1126299580272
multiplying two matrices... 220
 intermediate answer with bit 1 1326232513692
multiplying two matrices... 221
 intermediate answer with bit 1 1160667584928
multiplying two matrices... 222
 intermediate answer with bit 1 1638827732196
multiplying two matrices... 223
 intermediate answer with bit 1 996034851540
multiplying two matrices... 224
 intermediate answer with bit 1 1765898022852
multiplying two matrices... 225
 intermediate answer with bit 1 1439128668168
multiplying two matrices... 226
 intermediate answer with bit 1 1647606790332
multiplying two matrices... 227
 intermediate answer with bit 1 1096462512612
multiplying two matrices... 228
 intermediate answer with bit 1 2217162494736
multiplying two matrices... 229
 intermediate answer with bit 1 1836712584360
multiplying two matrices... 230
 intermediate answer with bit 1 1309466487816
multiplying two matrices... 231
 intermediate answer with bit 1 1523435566680
multiplying two matrices... 232
 intermediate answer with bit 1 1365884025120
multiplying two matrices... 233
 intermediate answer with bit 1 1202396683536
multiplying two matrices... 234
 intermediate answer with bit 1 1612996049988
multiplying two matrices... 235
 intermediate answer with bit 1 1468724711148
multiplying two matrices... 236
 intermediate answer with bit 1 2093448181248
multiplying two matrices... 237
 intermediate answer with bit 1 1437315838740
multiplying two matrices... 238
 intermediate answer with bit 1 1984237107984
multiplying two matrices... 239
 intermediate answer with bit 1 1414020232116
multiplying two matrices... 240
 intermediate answer with bit 1 1519534087956
multiplying two matrices... 241
 intermediate answer with bit 1 1361082457692
multiplying two matrices... 242
 intermediate answer with bit 1 614290957020
multiplying two matrices... 243
 intermediate answer with bit 1 1061784279756
multiplying two matrices... 244
 intermediate answer with bit 1 1211780569500
multiplying two matrices... 245
 intermediate answer with bit 1 1268818575984
multiplying two matrices... 246
 intermediate answer with bit 1 1091357138664
multiplying two matrices... 247
 intermediate answer with bit 1 613638227520
multiplying two matrices... 248
 intermediate answer with bit 1 1732478139936
multiplying two matrices... 249
 intermediate answer with bit 1 2025421555428
multiplying two matrices... 250
 intermediate answer with bit 1 1339270046424
multiplying two matrices... 251
 intermediate answer with bit 1 846037206684
multiplying two matrices... 252
 intermediate answer with bit 1 1717233670116
multiplying two matrices... 253
 intermediate answer with bit 1 1061199497628
multiplying two matrices... 254
 intermediate answer with bit 1 840617955576
multiplying two matrices... 255
Final answer 920321499
bonusSolution  : List[Unit] = List(())
*/
   
   
   
   
   
   
 /*
    val allMs = (0 to 22 ).scanLeft(T)((m, i) => matrixSquared(m))
    println("here2")
    println(allMs)
    println("allMs.size " + allMs.size)
    val droppedSecondEntry = allMs.head +: allMs.drop(2)
    println("here3")
    println(droppedSecondEntry)
    println("droppedSecondEntry.size " + droppedSecondEntry.size)
 //   val finalMatrix = droppedSecondEntry.reduceLeft(matrixTimesMatrix)
    val finalRow = droppedSecondEntry.foldLeft(Array.fill(three_States.size)(BigInt("1")))(rowTimesMatrix)
    println("here4")
    println(finalRow)
    val finalTotal = finalRow.sum
    println("here5")
    println(finalTotal)
    */
    
    /*here5
  with 0 to 23
2995505946

with 0 to 3 and Long. No good. See lots of negative values.
307057122

1949859552 with bigInts


here5 hurray! With 0 to 1. So that gets us 2^3 = 8. So to get 2^24 we're looking at 22.
17342172


  */

  //}
}