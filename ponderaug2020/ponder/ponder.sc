import com.sun.xml.internal.bind.util.Which
object ponder {
 
 val maxLimit = 100                               //> maxLimit  : Int = 100
 
  val allTriplets = for {
  a <- 0 to maxLimit
  b <- a to maxLimit
  c <- b to maxLimit} yield List(a,b,c)           //> allTriplets  : scala.collection.immutable.IndexedSeq[List[Int]] = Vector(Lis
                                                  //| t(0, 0, 0), List(0, 0, 1), List(0, 0, 2), List(0, 0, 3), List(0, 0, 4), List
                                                  //| (0, 0, 5), List(0, 0, 6), List(0, 0, 7), List(0, 0, 8), List(0, 0, 9), List(
                                                  //| 0, 0, 10), List(0, 0, 11), List(0, 0, 12), List(0, 0, 13), List(0, 0, 14), L
                                                  //| ist(0, 0, 15), List(0, 0, 16), List(0, 0, 17), List(0, 0, 18), List(0, 0, 19
                                                  //| ), List(0, 0, 20), List(0, 0, 21), List(0, 0, 22), List(0, 0, 23), List(0, 0
                                                  //| , 24), List(0, 0, 25), List(0, 0, 26), List(0, 0, 27), List(0, 0, 28), List(
                                                  //| 0, 0, 29), List(0, 0, 30), List(0, 0, 31), List(0, 0, 32), List(0, 0, 33), L
                                                  //| ist(0, 0, 34), List(0, 0, 35), List(0, 0, 36), List(0, 0, 37), List(0, 0, 38
                                                  //| ), List(0, 0, 39), List(0, 0, 40), List(0, 0, 41), List(0, 0, 42), List(0, 0
                                                  //| , 43), List(0, 0, 44), List(0, 0, 45), List(0, 0, 46), List(0, 0, 47), List(
                                                  //| 0, 0, 48), List(0, 0, 49), List(0, 0, 50), List(0, 0, 51), List(0, 0, 52), L
                                                  //| ist(0, 0, 53), List(0, 0
                                                  //| Output exceeds cutoff limit.
  
  allTriplets.size                                //> res0: Int = 176851
 /*  The rules of the game consists of two lists of positions, marked LOSE and WIN. In the default set of rules, LOSE contains the
     single position (0,0,0) and WIN is empty. A player whose turn starts at a LOSE position loses; starting at a WIN position wins;
     otherwise a move is made and the turn passed to the other player.
     A winning position is one where the current player can force a win (in one or more steps) by playing correctly; a losing position is a non-winning position.
     Out of the total 176851 positions, exactly 1264 positions are losing. Our goal is to lower this number to at most 1252 by finding
     an alternative set of rules (LOSE and WIN sets). As an example, adding (1,1,1) to LOSE increases the number of losing positions in the game to 1269.
  */
  
  
  List(0,1,2).combinations(1).foreach(println)    //> List(0)
                                                  //| List(1)
                                                  //| List(2)
  List(0,1,2).combinations(2).foreach(println)    //> List(0, 1)
                                                  //| List(0, 2)
                                                  //| List(1, 2)
  List(0,1,2).combinations(3).foreach(println)    //> List(0, 1, 2)
  
  
  def allDestinations( inUnsorted: List[Int] ) = {
   val in = inUnsorted.sorted
   
   // take from all 3
   // We'll take n coins, from numToTakeFrom people, who are specified by whichIndexesToTakeFrom
   val firstPart = for {
    n <- 1 to in(0)
    numToTakeFrom <- List(1,2,3)
    whichIndexesToTakeFrom <- List(0,1,2).combinations(numToTakeFrom)
   } yield in.zipWithIndex.map( x => if (whichIndexesToTakeFrom.contains(x._2)) x._1 - n else x._1 )
  
   // take from only 2 of them
   // We'll take n coins, from numToTakeFrom people, who are specified by whichIndexesToTakeFrom
   val secondPart = for {
    n <- in(0) +1 to in(1)
    numToTakeFrom <- List(1,2)
    whichIndexesToTakeFrom <- List(1,2).combinations(numToTakeFrom)
   } yield in.zipWithIndex.map( x => if (whichIndexesToTakeFrom.contains(x._2)) x._1 - n else x._1 )
  
   // take from only the biggest one
   // We'll take n coins, from numToTakeFrom people, who are specified by whichIndexesToTakeFrom
  val thirdPart = for {
    n <- in(1) +1 to in(2)
    numToTakeFrom <- List(1)
    whichIndexesToTakeFrom <- List(2).combinations(numToTakeFrom)
   } yield in.zipWithIndex.map( x => if (whichIndexesToTakeFrom.contains(x._2)) x._1 - n else x._1 )
  
  thirdPart
   
  (firstPart.map(_.sorted) ++ secondPart.map(_.sorted) ++ thirdPart.map(_.sorted)).distinct
 
 }                                                //> allDestinations: (inUnsorted: List[Int])scala.collection.immutable.IndexedS
                                                  //| eq[List[Int]]
  allDestinations( List(2,1,4) )                  //> res1: scala.collection.immutable.IndexedSeq[List[Int]] = Vector(List(0, 2, 
                                                  //| 4), List(1, 1, 4), List(1, 2, 3), List(0, 1, 4), List(0, 2, 3), List(1, 1, 
                                                  //| 3), List(0, 1, 3), List(1, 2, 2), List(0, 1, 2), List(1, 1, 2))
  
  val winOrLose = collection.mutable.Map[ List[Int], Boolean ]( List(0,0,0) -> false, List(1,1,1) -> false )
                                                  //> winOrLose  : scala.collection.mutable.Map[List[Int],Boolean] = Map(List(0, 
                                                  //| 0, 0) -> false, List(1, 1, 1) -> false)
  
  
  // A winning position is one where the current player can force a win (in one or more steps) by playing correctly; a losing position is a non-winning position.
  // That means a winning position is one where there exist a move that is a loser.
  val winners = allTriplets.foreach{ t =>
  // allDestinations should be in the cache, so we'll just go with the get
  	if (!winOrLose.contains(t))
     winOrLose.update(t , allDestinations(t).exists( dest => winOrLose(dest) == false ))
  }                                               //> winners  : Unit = ()
  
//  winOrLose.foreach(println)
/*
// Number of losers
winOrLose.count(_._2 == false)
winOrLose.count(_._2 == true)
winOrLose.size
*/
// Number of losers
/*
 winOrLose.count(_._2 == false) returns 1269
 winOrLose.count(_._2 == true)  returns 175582
 winOrLose.size                 returns 176851
*/
/* Out of the total 176851 positions, exactly 1264 positions are losing. Our goal is to lower this number to at most 1252 by finding
 an alternative set of rules (LOSE and WIN sets). As an example, adding (1,1,1) to LOSE increases the number of losing positions in the game to 1269.
 */
 // Our goal is to lower this number to at most 1252 by finding
 /*
 type Torus = Set[Point]
  
  def neighbourOfTorus(x: Torus) : Torus = {
  val xp = scala.util.Random.nextInt(11)
  val yp = scala.util.Random.nextInt(11)
  if (x.contains(xp,yp))
   x - ((xp,yp))
  else
   x + ((xp,yp))
 }
  */
  type WinnersAndLosers = ( List[ List[Int]],List[ List[Int]] )
  
  def whileLoop(cond : =>Boolean)(block : =>Unit) : Unit =
  if(cond) {
    block
    whileLoop(cond)(block)
  }                                               //> whileLoop: (cond: => Boolean)(block: => Unit)Unit
  
 
 def neighbour( x: WinnersAndLosers) : WinnersAndLosers  = {
  val pickWinner = scala.util.Random.nextBoolean()
  var whichPlayerToPick : Int = 0
  var oneTwoOrThree : Int = 0
  var upOrDown : Boolean = false
  
  if (pickWinner)
  {
   do {
    whichPlayerToPick = scala.util.Random.nextInt(x._1.size)
    oneTwoOrThree = scala.util.Random.nextInt(3)
    upOrDown = scala.util.Random.nextBoolean()
   } while ( !(x._1(whichPlayerToPick)(oneTwoOrThree) == 100 &&  upOrDown ) &&
             !(x._1(whichPlayerToPick)(oneTwoOrThree) == 0   && !upOrDown)
             // && This isn't already a winner? ....!(x._1(whichPlayerToPick)(oneTwoOrThree)
             
             
               )
            
  ( x._1.updated( whichPlayerToPick, x._1(whichPlayerToPick).updated(oneTwoOrThree, x._1(whichPlayerToPick)(oneTwoOrThree) + (if (upOrDown) 1 else -1)) ), x._2)
    
   
  }
  else
  {
   do {
    whichPlayerToPick = scala.util.Random.nextInt(x._2.size)
    oneTwoOrThree = scala.util.Random.nextInt(3)
    upOrDown = scala.util.Random.nextBoolean()
  } while ( !(x._2(whichPlayerToPick)(oneTwoOrThree) == 100 &&  upOrDown ) &&
             !(x._2(whichPlayerToPick)(oneTwoOrThree) == 0   && !upOrDown)   )
  ( x._1, x._2.updated( whichPlayerToPick, x._2(whichPlayerToPick).updated(oneTwoOrThree, x._2(whichPlayerToPick)(oneTwoOrThree) + (if (upOrDown) 1 else -1)) ))
  }
 }                                                //> neighbour: (x: ponder.WinnersAndLosers)ponder.WinnersAndLosers
 
 
 // initilizale random winners and losers
 // Both WIN and LOSE should be of size at most 10.
 var wol = (
            List( List(1,2,3), List(10,11,42), List(20,41,62) ),
            List( List( 8,23,38), List(28, 39,72), List( 45, 67, 88), List( 15,25,35) ))
                                                  //> wol  : (List[List[Int]], List[List[Int]]) = (List(List(1, 2, 3), List(10, 1
                                                  //| 1, 42), List(20, 41, 62)),List(List(8, 23, 38), List(28, 39, 72), List(45, 
                                                  //| 67, 88), List(15, 25, 35)))
   
 
 
 // some loop until
 var didWeWin = false                             //> didWeWin  : Boolean = false|
 
 do {
   winOrLose.empty
   wol._1.foreach(x => winOrLose.update(x, true))
   wol._2.foreach(x => winOrLose.update(x, false))
	// We always need this to be a Loser
   winOrLose.update(List(0,0,0), false)
  
  allTriplets.foreach{ t =>
  // allDestinations should be in the cache, so we'll just go with the get
  	if (!winOrLose.contains(t))
     winOrLose.update(t , allDestinations(t).exists( dest => winOrLose(dest) == false ))
  }
  
  didWeWin = winOrLose.count(_._2 == false) <= 1252
  
  if (didWeWin)
  {
  println("Solution found!!!")
  println("Winners")
  println(wol._1)
  println("Losers")
  println(wol._2)
  }
  else
  {
   wol = neighbour(wol)
  }
  
  
 } while ( !didWeWin  )
}