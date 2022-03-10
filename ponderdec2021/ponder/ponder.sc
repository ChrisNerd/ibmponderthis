object ponder {
 /*
 val n = 5
 val m = 15
 val sigma = List(0,1,4,3,2)
 */

 /*
 Your goal: For n=9 and m=150, Given the permutation [5, 6, 4, 7, 3, 8, 2, 9, 1],
 find an initial orientation of the pins of the board such that the resulting score is at least 5.


 A bonus "*" will be given for finding a score of at least 20
 for n=10 m=150 and permutation [4, 8, 2, 7, 10, 6, 3, 9, 1, 5]
  */
 val n=10                                         //> n  : Int = 10
 val m=150                                        //> m  : Int = 150
 val sigma = List(4, 8, 2, 7, 10, 6, 3, 9, 1, 5)// List(5, 6, 4, 7, 3, 8, 2, 9, 1)
                                                  //> sigma  : List[Int] = List(4, 8, 2, 7, 10, 6, 3, 9, 1, 5)
 val scoreToBeat = 20                             //> scoreToBeat  : Int = 20
 
 // mid, then 6-4 are next two centre ones, then 7-3 next furthest from centre ...
 // This is maximum hard!
 // Maybe there's an easy solution. Symmetric to stear it towards the sides.
 // by the first one being a R, then second row is < >
 // third row is < x >
 // fourth is   < x x >
 // then the rest is random. That would give a nice bimodal distribution. Like 14641 14641.
 // How can we get 9 and 1 better than 1? Put a < above it?
 // How about
 /*
 
     R
    x y
   <   >
  <     >
 <       >

     R
    x y
   <   >
  < < > >
 <       >

1 will get all of x/2.
9 will get all of y/2.
The rest will still get a distribution

R x y, < R R >, < RRR >, < RRRR >, < RRRRR>, < RRRRRR> only need 6 rs in the middle. total of 8 pegs, which gives 9 outputs.
 
 d/dx((a + x)^4 (c - x)^5 - a^4 c^5) = (a + x)^3 (c - x)^4 (-5 a + 4 c - 9 x)
 
 x = (4 c)/9 - (5 a)/9
 
 Ahhh, using Lagrange multipliers
 
 we have the function we want to maximize, score = f(a), and the constraint g(a) = 0 = -m + sum(a)
 L(a,lambda) = f(a) - lambda * g(a)
 
 f(a) = a_1^1 * a_2^2 * a_3^3 .... a_9^9 (ignoring the m and n terms since they are constants)
 
 We solve the lagrangian by setting the gradiant to 0.
 the gradiant is just d/da1, d/da2,... d/da9, d/d lambda
 
 d/da1 (f - lambda g) = a_2^2 *a_3^3...a_9^9 - lambda   // the lambda term is because g(a) = -m + a_1 + a_2 + a_3...+ a_9, so dg/da_i = 1

we'll end up with 9 equations like
a_2^2 *a_3^3...a_9^9 = lambda (1)
a_1 * 2 a_2 *a_3^3...a_9^9 = lambda  (2)
a_1 * a_2^2 *3a_3^2...a_9^9 = lambda  (3)
...

We can do (2)/(1) to get a_2/2a_1 = 1  => a_2=2a_1
and likewise (3)/(1)  gives a_3 = 3a1
and (i)/(1) gives =a_i = i * a_1

So the a terms are linear! Surprised, I would have guessed they were geometric.
Anyways, we can use the constraint m= sum(a) to finish up.
m=a_1 + 2a_1 + 3a_1 + ... 9a_1 = 45 a_1.
So a_1 = m/45, a_2= 2m/45, a_3 = 3m/45.

Letting m=150 like the first question, we round our a values to
[3 7 10 13 17 20 23 27 30],
which gives a score of 750.616...
Not limiting ourselves to integers,
a1=10/3, a2=20/3 a3=30/3=10 etc gives
score=f=759.2063128989328779502486372232936728559616
 
 
 
 */
 
 assert(n ==sigma.size)
 

  
 def score(a: List[Int],  sigma: List[Int]): Double =
  sigma.zipWithIndex.map{ case(sigmak, k) => scala.math.pow( a(sigmak-1).toDouble * n /m, k.toDouble + 1) }.product
                                                  //> score: (a: List[Int], sigma: List[Int])Double
  
 score( List(1,3,6,4,1), List(1,2,5,4,3))         //> res0: Double = 4.091380216430424E-11

 // Should be 1.25
 
 /*
  Let's see. sigma = 0,1,4,3,2. Is really the 0 based indexes.
  a(0)=1,
  a(1)=3
  a(4)=1
  a(3)=4
  a(2)=6
 */
 
 
 def printBoard(board: List[Char]) = {
  for( currentRow <- 0 until n)
   println(" " * (n - currentRow - 1) + {
    for{index <- 0 until currentRow}
     yield(board(index+ currentRow*(currentRow-1)/2))
    }.mkString(" ")
   )
 }                                                //> printBoard: (board: List[Char])Unit
 val debug = false                                //> debug  : Boolean = false
  
 def galton(board: List[Char], currentRow: Int = 0, currentIndex: Int = 0): (Int, List[Char]) = {
 if (debug) {
  println("board")
  printBoard(board)
  }
  val newBoard = board(currentIndex) match {
   case 'R' => board.updated(currentIndex, 'L')
   case 'L' => board.updated(currentIndex, 'R')
   case '<' => board
   case '>' => board
  }
  val nextIndex =    currentIndex + (if (List('R', '>') contains board(currentIndex) ) currentRow + 2  else  currentRow + 1)
  if (debug)
  {
   println("Newboard")
   printBoard(newBoard)
   println("currentRow " + currentRow +  " currentIndex " + currentIndex + " nextIndex " + nextIndex)
  }
  
  if (currentRow == n - 2)
   (nextIndex - n*(n-1)/2, newBoard)
  else
   galton(newBoard, currentRow + 1, nextIndex)
 }                                                //> galton: (board: List[Char], currentRow: Int, currentIndex: Int)(Int, List[C
                                                  //| har])
 
  
 def runSingleGalton( runningListOfResults_Board_pair: (List[Int], List[Char]), currentRunNumber: Int ) =
 {
   val (galtonIndexOut, newBoard) = galton( runningListOfResults_Board_pair._2 )
   (runningListOfResults_Board_pair._1.updated( galtonIndexOut, runningListOfResults_Board_pair._1(galtonIndexOut) + 1), newBoard)
 }                                                //> runSingleGalton: (runningListOfResults_Board_pair: (List[Int], List[Char]),
                                                  //|  currentRunNumber: Int)(List[Int], List[Char])
 
 def runEntireMGaltons( in: List[Char] ) = (1 to m).foldLeft( List.fill(n)(0), in )( runSingleGalton )
                                                  //> runEntireMGaltons: (in: List[Char])(List[Int], List[Char])
 
 // val initialList = List.fill(n*(n-1)/2)('R')
// val initialList = List.fill(n*(n-1)/2)( Set('R', 'L', '<', '>').iterator.drop( scala.util.Random.nextInt(4)).next )
 //val initialList = List.fill(n*(n-1)/2)( Set('R', 'L' ).iterator.drop( scala.util.Random.nextInt(2)).next )

 //initialList.size
 val initialList = List(
            'L',            // 1
          'L','R',          // 2
        'L','L','R',        // 3
      '<','L','R','>',          // 4
    '<','L','L','R','>',          // 5
  '<','<','L','R','>','>',          // 6
 '<','<','L','L','R','>','>',          // 7
'<','<','<','L','L','R','>','>',          // 8
)                                                 //> initialList  : List[Char] = List(L, L, R, L, L, R, <, L, R, >, <, L, L, R, 
                                                  //| >, <, <, L, R, >, >, <, <, L, L, R, >, >, <, <, <, L, L, R, >, >)


 val changeableList = List(
            'x',            // 1
          'x','x',          // 2
        'x','x','x',        // 3
      '<','L','R','>',          // 4
    '<','L','L','R','>',          // 5
  '<','<','L','R','>','>',          // 6
 '<','<','L','L','R','>','>',          // 7
'<','<','x','L','L','x','>','>',          // 8
)                                                 //> changeableList  : List[Char] = List(x, x, x, x, x, x, <, L, R, >, <, L, L, 
                                                  //| R, >, <, <, L, R, >, >, <, <, L, L, R, >, >, <, <, x, L, L, x, >, >)

/*should use changeableList to make neighbour.
maybe x can be RL
y can be RL<>
*/
 assert(changeableList.size == initialList.size)

// assert(n*(n-1)/2 == initialList.size)

 //val foldresult = (1 to m).foldLeft( List.fill(n)(0), initialList )( runSingleGalton )
 
 def runAndScore( in: List[Char] ): Double = {
  val foldresult = runEntireMGaltons(in)
  score(foldresult._1, sigma)
 }                                                //> runAndScore: (in: List[Char])Double
 
 //val initres = runEntireMGaltons(initialList)
 //printBoard(initres._2)
 //runAndScore(initialList)
 
 
 def neighbourOfInput( input: List[Char]) : List[Char] = {
  /*
  val c = changeableList.zipWithIndex.filter( List('x','y','L','R') contains _._1)  // excludes the < and >
  val randomIndexInChangable = scala.util.Random.nextInt( c.size )
  val replaceWith = c(randomIndexInChangable) match {
   case ('x', ind) =>  if (input(ind) == 'L') 'R' else 'L'
   case ('R', ind) => (Set('R', 'L', '<', '>') - input(ind)).iterator.drop(scala.util.Random.nextInt(3) ).next
   case ('L', ind) => (Set('R', 'L', '<', '>') - input(ind)).iterator.drop(scala.util.Random.nextInt(3) ).next
  }
 */
 
  val randomIndexInOutputChoices = scala.util.Random.nextInt( 4 - 1 )
  //val randomIndexInOutputChoices = scala.util.Random.nextInt( 2 - 1 )
 /* val replaceWith =
  if (scala.util.Random.nextDouble > .95)
   (Set('R', 'L', '<', '>') - input(randomIndexInInput)).iterator.drop(randomIndexInOutputChoices).next
  else
    (Set('R', 'L') - input(randomIndexInInput)).iterator.drop(scala.util.Random.nextInt(1) ).next
  */
  val randomIndexInInput = scala.util.Random.nextInt( input.size )
  
  val replaceWith = (Set('R', 'L','<','>') - input(randomIndexInInput)).iterator.drop(randomIndexInOutputChoices).next
  // input.updated(c(randomIndexInChangable)._2, replaceWith)
  input.updated(randomIndexInInput, replaceWith)
 }                                                //> neighbourOfInput: (input: List[Char])List[Char]
  
 val (initialTemperature, finalTemperature, coolingRate) = (1.0, 0.0001, 0.0005)
                                                  //> initialTemperature  : Double = 1.0
                                                  //| finalTemperature  : Double = 1.0E-4
                                                  //| coolingRate  : Double = 5.0E-4
  
 def simulatedAnnealing( best: List[Char], temp:Double): List[Char] = {
  if (temp > finalTemperature) {
   val currentEnergy = runAndScore(best)
   val neighbour = neighbourOfInput( best )
   val neighbourEnergy = runAndScore(neighbour)
   //println(temp)
   if (neighbourEnergy >= scoreToBeat)
   {
    println("Solution!!!")
    println(neighbour)
    neighbour
   }
   else
   {   // Decide if we should accept the neighbour
    val acceptanceProbability = math.exp( (neighbourEnergy - currentEnergy)/temp)
    val accept = (acceptanceProbability > math.random)
 //   println("neighbourEnergy" + neighbourEnergy + "currentEnergy" + currentEnergy + "acceptProb" + acceptanceProbability)
    
    simulatedAnnealing( if (accept) neighbour else best, (1-coolingRate)*temp)
   }
  } else best
 }                                                //> simulatedAnnealing: (best: List[Char], temp: Double)List[Char]
 
 val bonusSolution = List('L', 'R', '>', '<', '<', 'L', '<', 'L', '<', '>', '<', 'R', '<', '<', '>', '<', '<', 'L', 'R', 'L', 'L', '<', 'L', 'L', 'R', '>', 'R', 'R', 'L', 'R', 'R', '>', '<', 'R', '>', '>', '<', 'R', '<', 'L', 'L', '>', 'R', '>', '>')
                                                  //> bonusSolution  : List[Char] = List(L, R, >, <, <, L, <, L, <, >, <, R, <, <
                                                  //| , >, <, <, L, R, L, L, <, L, L, R, >, R, R, L, R, R, >, <, R, >, >, <, R, <
                                                  //| , L, L, >, R, >, >)
 printBoard(bonusSolution)                        //>          
                                                  //|         L
                                                  //|        R >
                                                  //|       < < L
                                                  //|      < L < >
                                                  //|     < R < < >
                                                  //|    < < L R L L
                                                  //|   < L L R > R R
                                                  //|  L R R > < R > >
                                                  //| < R < L L > R > >
 runEntireMGaltons(bonusSolution)                 //> res1: (List[Int], List[Char]) = (List(19, 10, 23, 6, 30, 25, 6, 3, 19, 9),L
                                                  //| ist(L, L, >, <, <, R, <, L, <, >, <, L, <, <, >, <, <, R, R, L, R, <, R, L,
                                                  //|  L, >, L, R, R, L, L, >, <, L, >, >, <, R, <, R, L, >, L, >, >))
 println(runAndScore(bonusSolution))              //> 229.58881582203665
 
 val solution = Stream.from(0).find{ trial =>
  val simulatedAnnealingResult = simulatedAnnealing(
   List.fill(n*(n-1)/2)( Set('R', 'L', '<', '>').iterator.drop( scala.util.Random.nextInt(4)).next ),
   //initialList,
   initialTemperature)
  val result = runEntireMGaltons(simulatedAnnealingResult)

  val s = runAndScore(simulatedAnnealingResult)
  if (trial % 100 == 0)
   {
    printBoard(simulatedAnnealingResult)
    printBoard(result._2)
    println(result._1)
    println(s)
   }
  s >= scoreToBeat
  }

}