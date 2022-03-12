object ponder {
 type Point = (Int,Int)
 type Torus = Set[Point]
 type Rules = Vector[Vector[Boolean]]
  
 def neighbours(p: Point) = List(
  (0, 1),
  (0,10),
  (1,0),
  (10,0)).map(offset=> ((p._1 -1 + offset._1) % 11 +1, (p._2-1 + offset._2) % 11 +1))
                                                  //> neighbours: (p: ponder.Point)List[(Int, Int)]
  
 def countNeighbours(p: Point, u: Torus) = neighbours(p).count( u.contains )
                                                  //> countNeighbours: (p: ponder.Point, u: ponder.Torus)Int
  
 def updateUniverse(u: Torus, rules: Rules): Torus =
  (for{
   x <- (1 to 11).toSet[Int]
   y <- 1 to 11
  } yield (x,y))
  .filter( p =>
   if(!u.contains(p))
    rules(0)(countNeighbours(p, u))
   else
    rules(1)(countNeighbours(p, u)))              //> updateUniverse: (u: ponder.Torus, rules: ponder.Rules)ponder.Torus

 def printUniverse(u: Torus) =
  for( y <- 11 to 1 by -1 )
   println( (for{x <- 1 to 11} yield if (u.contains(x,y)) '*' else '.').mkString )
                                                  //> printUniverse: (u: ponder.Torus)Unit
	
 val r1: Rules = Vector( Vector( false, true, false, true, false), Vector( false, true, true, true, false))
                                                  //> r1  : ponder.Rules = Vector(Vector(false, true, false, true, false), Vector(
                                                  //| false, true, true, true, false))
 val x: Torus = Set( (5,5))                       //> x  : ponder.Torus = Set((5,5))
 printUniverse(x)                                 //> ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ....*......
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
 printUniverse(updateUniverse(x, r1))             //> ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ....*......
                                                  //| ...*.*.....
                                                  //| ....*......
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
 //Function.chain(List.fill(10)(updateVector1Day(M)))
 val m : scala.collection.mutable.Map[ Torus, Int ] = scala.collection.mutable.Map()
                                                  //> m  : scala.collection.mutable.Map[ponder.Torus,Int] = Map()
 def findPeriod(x: Torus, r: Rules, i: Int): (Int, Int) =
 {
 //println(i + " " + m.size)
  if (i == 0)
    m.clear
    
  if (i == 0 || i == 1 || i == 2 || i <= 10 || i == 100 || i == 1000 || (i >= 6196 - 6 && i <= 6200))//(i%100000 == 0)
   {
/*   println(i)
   printUniverse(x)
  */
  }
  /*if (x.isEmpty || i > 100072)
  {
    m.clear()
   (-1, i)
  }
  else*/
  // Got to remove this after the stats are calculated
  if ( i > 500000 )
   {
   println("No period found")
   m.clear()
   (50, i)
  }
  else
  {
   val xnew = updateUniverse(x, r)
   if (m.contains(xnew))
    if ( i - m(xnew) == 72 && i >= 100000)
    {
     println("Solution Found!!!")
     println("Rules " + r)
     println("i " + i)
     m.clear()
     (72,i)
    }
    else
    {
     val transient = i - m(xnew)
     m.clear()
     (transient, i)
     }
   else
   {
    m.update(xnew, i)
    findPeriod(xnew,r, i+1)
   }
  }
 }                                                //> findPeriod: (x: ponder.Torus, r: ponder.Rules, i: Int)(Int, Int)
 findPeriod(x, r1, 0)                             //> res0: (Int, Int) = (3,19)

 def neighbourOfTorus(x: Torus) : Torus = {
  val xp = scala.util.Random.nextInt(11)
  val yp = scala.util.Random.nextInt(11)
  if (x.contains(xp,yp))
   x - ((xp,yp))
  else
   x + ((xp,yp))
 }                                                //> neighbourOfTorus: (x: ponder.Torus)ponder.Torus
  
 val (initialTemperature, finalTemperature, coolingRate) = (200.0, 0.1, 0.005)
                                                  //> initialTemperature  : Double = 200.0
                                                  //| finalTemperature  : Double = 0.1
                                                  //| coolingRate  : Double = 0.005
 
 /*
 def simulatedAnnealing( best: List[List[Int]], temp:Double): List[List[Int]] = {
  println("Temp " + temp)
  if (temp > finalTemperature) {
   val currentEnergy = determinant(best)
   val neighbour = createNeighbour( best )
   val neighbourEnergy = determinant(neighbour)
   if (neighbourEnergy > scoreToBeat)
   {
    println("Solution!!!")
    println(neighbour)
    neighbour
   }
   else
   {   // Decide if we should accept the neighbour
    val accept = (math.exp((neighbourEnergy - currentEnergy)/temp) > math.random)
    simulatedAnnealing( if (accept) {
     // println("\nScore: " + neighbourEnergy, " Temperature: "+ temp)
     neighbour
     } else best, (1-coolingRate)*temp)
   }
  } else best
 }
 
 */
 
 def calcEnergy( perTransitentTime: (Int,Int) ) =
 {
   if (perTransitentTime._2 >= 100000)
    if (perTransitentTime._1 == 72)  // This is redundant!
     0
    else
     scala.math.abs( 72 - perTransitentTime._1)
   else
    scala.math.abs( 72 - perTransitentTime._1) + scala.math.abs(100000 - perTransitentTime._2)
 }                                                //> calcEnergy: (perTransitentTime: (Int, Int))Int
 
 def createNeighbour(best: (Torus, Rules)) =
 {
  val randomPoint = (scala.util.Random.nextInt(11) , scala.util.Random.nextInt(11))
  val newTorus =
   if (best._1.isEmpty)
    best._1 + randomPoint
   else
    best._1 - best._1.head + randomPoint


  val newRules =
   if (math.random <= 0.0001)
   {
    val randomRuleIndex = (scala.util.Random.nextInt(2) , scala.util.Random.nextInt(5))
    best._2.updated(randomRuleIndex._1, best._2(randomRuleIndex._1).updated(randomRuleIndex._2,
     !best._2(randomRuleIndex._1)(randomRuleIndex._2) ))
   }
   else
    best._2
  (newTorus, newRules)
 }                                                //> createNeighbour: (best: (ponder.Torus, ponder.Rules))(scala.collection.immu
                                                  //| table.Set[ponder.Point], scala.collection.immutable.Vector[Vector[Boolean]]
                                                  //| )
 
 def simulatedAnnealing( best: (Torus, Rules), temp: Double, oldScore : Int = 100000 ): (Torus,Rules) = {
  //println("Temp " + temp)
  if (temp > finalTemperature) {
   val currentEnergy = oldScore // calcEnergy(findPeriod( best._1, best._2, 0))
   val neighbour = createNeighbour(best)
   val neighbourEnergy = calcEnergy(findPeriod(neighbour._1, neighbour._2, 0))
   println("currentEnergy " + currentEnergy + " neighbourEnergy " + neighbourEnergy + " temp " + temp + " neigh - curr " + (neighbourEnergy - currentEnergy) + " e^-(n-c)/t " + math.exp(-(neighbourEnergy - currentEnergy)/temp) )
   
   if (neighbourEnergy == 0)
   {
    println("Solution!!!")
    println(neighbour)
    neighbour
   }
   else
   {
    // Decide if we should accept the neighbour
    val accept = (math.exp(-(neighbourEnergy - currentEnergy)/temp) > math.random)
    simulatedAnnealing( if (accept) {
     // println("\nScore: " + neighbourEnergy, " Temperature: "+ temp)
     neighbour
     } else best, (1-coolingRate)*temp, if (accept) neighbourEnergy else currentEnergy)
   }
  } else best
 }                                                //> simulatedAnnealing: (best: (ponder.Torus, ponder.Rules), temp: Double, oldS
                                                  //| core: Int)(ponder.Torus, ponder.Rules)


 val s = ((0 to 4).toSet.subsets.map{ setOfInts => (0 to 4).map{ i => setOfInts contains i }}).toVector
                                                  //> s  : Vector[scala.collection.immutable.IndexedSeq[Boolean]] = Vector(Vector
                                                  //| (false, false, false, false, false), Vector(true, false, false, false, fals
                                                  //| e), Vector(false, true, false, false, false), Vector(false, false, true, fa
                                                  //| lse, false), Vector(false, false, false, true, false), Vector(false, false,
                                                  //|  false, false, true), Vector(true, true, false, false, false), Vector(true,
                                                  //|  false, true, false, false), Vector(true, false, false, true, false), Vecto
                                                  //| r(true, false, false, false, true), Vector(false, true, true, false, false)
                                                  //| , Vector(false, true, false, true, false), Vector(false, true, false, false
                                                  //| , true), Vector(false, false, true, true, false), Vector(false, false, true
                                                  //| , false, true), Vector(false, false, false, true, true), Vector(true, true,
                                                  //|  true, false, false), Vector(true, true, false, true, false), Vector(true, 
                                                  //| true, false, false, true), Vector(true, false, true, true, false), Vector(t
                                                  //| rue, false, true, false, true), Vector(true, false, false, true, true), Vec
                                                  //| tor(false, true, true, true, false), Vector(false, true, true, false, true)
                                                  //| , Vector(false, true, false, true, true), Vector(false, false, true, true, 
                                                  //| true), Vector(true, true, true, true, false), Vector(true, true, true, fals
                                                  //| e, true), Vector(true, true, false, true, true), Vector(true, false, true, 
                                                  //| true, true), Vector(false, true, true, true, true), Vector(true, true, true
                                                  //| , true, true))
 s.toVector                                       //> res1: scala.collection.immutable.Vector[scala.collection.immutable.IndexedS
                                                  //| eq[Boolean]] = Vector(Vector(false, false, false, false, false), Vector(tru
                                                  //| e, false, false, false, false), Vector(false, true, false, false, false), V
                                                  //| ector(false, false, true, false, false), Vector(false, false, false, true, 
                                                  //| false), Vector(false, false, false, false, true), Vector(true, true, false,
                                                  //|  false, false), Vector(true, false, true, false, false), Vector(true, false
                                                  //| , false, true, false), Vector(true, false, false, false, true), Vector(fals
                                                  //| e, true, true, false, false), Vector(false, true, false, true, false), Vect
                                                  //| or(false, true, false, false, true), Vector(false, false, true, true, false
                                                  //| ), Vector(false, false, true, false, true), Vector(false, false, false, tru
                                                  //| e, true), Vector(true, true, true, false, false), Vector(true, true, false,
                                                  //|  true, false), Vector(true, true, false, false, true), Vector(true, false, 
                                                  //| true, true, false), Vector(true, false, true, false, true), Vector(true, fa
                                                  //| lse, false, true, true), Vector(false, true, true, true, false), Vector(fal
                                                  //| se, true, true, false, true), Vector(false, true, false, true, true), Vecto
                                                  //| r(false, false, true, true, true), Vector(true, true, true, true, false), V
                                                  //| ector(true, true, true, false, true), Vector(true, true, false, true, true)
                                                  //| , Vector(true, false, true, true, true), Vector(false, true, true, true, tr
                                                  //| ue), Vector(true, true, true, true, true))
 val allRules = for {
  firstRule <- s
  secondRule <- s
 } yield Vector(firstRule.toVector, secondRule.toVector)
                                                  //> allRules  : scala.collection.immutable.Vector[scala.collection.immutable.Ve
                                                  //| ctor[Vector[Boolean]]] = Vector(Vector(Vector(false, false, false, false, f
                                                  //| alse), Vector(false, false, false, false, false)), Vector(Vector(false, fal
                                                  //| se, false, false, false), Vector(true, false, false, false, false)), Vector
                                                  //| (Vector(false, false, false, false, false), Vector(false, true, false, fals
                                                  //| e, false)), Vector(Vector(false, false, false, false, false), Vector(false,
                                                  //|  false, true, false, false)), Vector(Vector(false, false, false, false, fal
                                                  //| se), Vector(false, false, false, true, false)), Vector(Vector(false, false,
                                                  //|  false, false, false), Vector(false, false, false, false, true)), Vector(Ve
                                                  //| ctor(false, false, false, false, false), Vector(true, true, false, false, f
                                                  //| alse)), Vector(Vector(false, false, false, false, false), Vector(true, fals
                                                  //| e, true, false, false)), Vector(Vector(false, false, false, false, false), 
                                                  //| Vector(true, false, false, true, false)), Vector(Vector(false, false, false
                                                  //| , false, false), Vector(true, false, false, false, true)), Vector(Vector(fa
                                                  //| lse, false, false, false, false), Vector(false, true, true, false, false)),
                                                  //|  Vector(Vector(false, false, false, false, false), Vector(false, true, fals
                                                  //| e, true, false)), Vector(Vector(false, false, false, false, false), Vector(
                                                  //| false, true, false, false, true)), Vector(Vector(false, false, false, false
                                                  //| , false), Vector(false, false, true, true, false)), Vector(Vector(false, fa
                                                  //| lse, false, false, false), Vector(false, false, true, false, true)), Vector
                                                  //| (Vector(false, false, false, false, false), Vector(false, false, false, tru
                                                  //| e, true)), Vector(Vector(false, false, false, false, false), Vector(true, t
                                                  //| rue, true, false, false)), Vector(Vector(false, false, false, false, false)
                                                  //| , Vector(true, true, false, true, false)), Vector(Vector(false, false, fals
                                                  //| e, false, false), Vector(true, true, false, false, true)), Vector(Vector(fa
                                                  //| lse, false, false, false, false), Vector(true, false, true, true, false)), 
                                                  //| Vector(Vector(false, false, false, false, false), Vector(true, false, true,
                                                  //|  false, true)), Vector(Vector(false, false, false, false, false), Vector(tr
                                                  //| ue, false, false, true, true)), Vector(Vector(false, false, false, false, f
                                                  //| alse), Vector(false, true, true, true, false)), Vector(Vector(false, false,
                                                  //|  false, false, false), Vector(false, true, true, false, true)), Vector(Vect
                                                  //| or(false, false, false, false, false), Vector(false, true, false, true, tru
                                                  //| e)), Vector(Vector(false, false, false, false, false), Vector(false, false,
                                                  //|  true, true, true)), Vector(Vector(false, false, false, false, false), Vect
                                                  //| or(true, true, true, true, false)), Vector(Vector(false, false, false, fals
                                                  //| e, false), Vector(true, true, true, false, true)), Vector(Vector(false, fal
                                                  //| se, false, false, false), Vector(true, true, false, true, true)), Vector(Ve
                                                  //| ctor(false, false, false, false, false), Vector(true, false, true, true, tr
                                                  //| ue)), Vector(Vector(false, false, false, false, false), Vector(false, true,
                                                  //|  true, true, true)), Vector(Vector(false, false, false, false, false), Vect
                                                  //| or(true, true, true, true, true)), Vector(Vector(true, false, false, false,
                                                  //|  false), Vector(false, false, false, false, false)), Vector(Vector(true, fa
                                                  //| lse, false, false, false), Vector(true, false, false, false, false)), Vecto
                                                  //| r(Vector(true, false, false, false, false), Vector(false, true, false, fals
                                                  //| e, false)), Vector(Vector(true, false, false, false, false), Vector(false, 
                                                  //| false, true, false, false)), Vector(Vector(true, false, false, false, false
                                                  //| ), Vector(false, false, false, true, false)), Vector(Vector(true, false, fa
                                                  //| lse, false, false), Vector(false, false, false, false, true)), Vector(Vecto
                                                  //| r(true, false, false, false, false), Vector(true, true, false, false, false
                                                  //| )), Vector(Vector(true, false, false, false, false), Vector(true, false, tr
                                                  //| ue, false, false)), Vector(Vector(true, false, false, false, false), Vector
                                                  //| (true, false, false, true, false)), Vector(Vector(true, false, false, false
                                                  //| , false), Vector(true, false, false, false, true)), Vector(Vector(true, fal
                                                  //| se, false, false, false), Vector(false, true, true, false, false)), Vector(
                                                  //| Vector(true, false, false, false, false), Vector(false, true, false, true, 
                                                  //| false)), Vector(Vector(true, false, false, false, false), Vector(false, tru
                                                  //| e, false, false, true)), Vector(Vector(true, false, false, false, false), V
                                                  //| ector(false, false, true, true, false)), Vector(Vector(true, false, false, 
                                                  //| false, false), Vector(false, false, true, false, true)), Vector(Vector(true
                                                  //| , false, false, false, false), Vector(false, false, false, true, true)), Ve
                                                  //| ctor(Vector(true, false, false, false, false), Vector(true, true, true, fal
                                                  //| se, false)), Vector(Vector(true, false, false, false, false), Vector(true, 
                                                  //| true, false, true, false)), Vector(Vector(true, false, false, false, false)
                                                  //| , Vector(true, true, false, false, true)), Vector(Vector(true, false, false
                                                  //| , false, false), Vector(true, false, true, true, false)), Vector(Vector(tru
                                                  //| e, false, false, false, false), Vector(true, false, true, false, true)), Ve
                                                  //| ctor(Vector(true, false, false, false, false), Vector(true, false, false, t
                                                  //| rue, true)), Vector(Vector(true, false, false, false, false), Vector(false,
                                                  //|  true, true, true, false)), Vector(Vector(true, false, false, false, false)
                                                  //| , Vector(false, true, true, false, true)), Vector(Vector(true, false, false
                                                  //| , false, false), Vector(false, true, false, true, true)), Vector(Vector(tru
                                                  //| e, false, false, false, false), Vector(false, false, true, true, true)), Ve
                                                  //| ctor(Vector(true, false, false, false, false), Vector(true, true, true, tru
                                                  //| e, false)), Vector(Vector(true, false, false, false, false), Vector(true, t
                                                  //| rue, true, false, true)), Vector(Vector(true, false, false, false, false), 
                                                  //| Vector(true, true, false, true, true)), Vector(Vector(true, false, false, f
                                                  //| alse, false), Vector(true, false, true, true, true)), Vector(Vector(true, f
                                                  //| alse, false, false, false), Vector(false, true, true, true, true)), Vector(
                                                  //| Vector(true, false, false, false, false), Vector(true, true, true, true, tr
                                                  //| ue)), Vector(Vector(false, true, false, false, false), Vector(false, false,
                                                  //|  false, false, false)), Vector(Vector(false, true, false, false, false), Ve
                                                  //| ctor(true, false, false, false, false)), Vector(Vector(false, true, false, 
                                                  //| false, false), Vector(false, true, false, false, false)), Vector(Vector(fal
                                                  //| se, true, false, false, false), Vector(false, false, true, false, false)), 
                                                  //| Vector(Vector(false, true, false, false, false), Vector(false, false, false
                                                  //| , true, false)), Vector(Vector(false, true, false, false, false), Vector(fa
                                                  //| lse, false, false, false, true)), Vector(Vector(false, true, false, false, 
                                                  //| false), Vector(true, true, false, false, false)), Vector(Vector(false, true
                                                  //| , false, false, false), Vector(true, false, true, false, false)), Vector(Ve
                                                  //| ctor(false, true, false, false, false), Vector(true, false, false, true, fa
                                                  //| lse)), Vector(Vector(false, true, false, false, false), Vector(true, false,
                                                  //|  false, false, true)), Vector(Vector(false, true, false, false, false), Vec
                                                  //| tor(false, true, true, false, false)), Vector(Vector(false, true, false, fa
                                                  //| lse, false), Vector(false, true, false, true, false)), Vector(Vector(false,
                                                  //|  true, false, false, false), Vector(false, true, false, false, true)), Vect
                                                  //| or(Vector(false, true, false, false, false), Vector(false, false, true, tru
                                                  //| e, false)), Vector(Vector(false, true, false, false, false), Vector(false, 
                                                  //| false, true, false, true)), Vector(Vector(false, true, false, false, false)
                                                  //| , Vector(false, false, false, true, true)), Vector(Vector(false, true, fals
                                                  //| e, false, false), Vector(true, true, true, false, false)), Vector(Vector(fa
                                                  //| lse, true, false, false, false), Vector(true, true, false, true, false)), V
                                                  //| ector(Vector(false, true, false, false, false), Vector(true, true, false, f
                                                  //| alse, true)), Vector(Vector(false, true, false, false, false), Vector(true,
                                                  //|  false, true, true, false)), Vector(Vector(false, true, false, false, false
                                                  //| ), Vector(true, false, true, false, true)), Vector(Vector(false, true, fals
                                                  //| e, false, false), Vector(true, false, false, true, true)), Vector(Vector(fa
                                                  //| lse, true, false, false, false), Vector(false, true, true, true, false)), V
                                                  //| ector(Vector(false, true, false, false, false), Vector(false, true, true, f
                                                  //| alse, true)), Vector(Vector(false, true, false, false, false), Vector(false
                                                  //| , true, false, true, true)), Vector(Vector(false, true, false, false, false
                                                  //| ), Vector(false, false, true, true, true)), Vector(Vector(false, true, fals
                                                  //| e, false, false), Vector(true, true, true, true, false)), Vector(Vector(fal
                                                  //| se, true, false, false, false), Vector(true, true, true, false, true)), Vec
                                                  //| tor(Vector(false, true, false, false, false), Vector(true, true, false, tru
                                                  //| e, true)), Vector(Vector(false, true, false, false, false), Vector(true, fa
                                                  //| lse, true, true, true)), Vector(Vector(false, true, false, false, false), V
                                                  //| ector(false, true, true, true, true)), Vector(Vector(false, true, false, fa
                                                  //| lse, false), Vector(true, true, true, true, true)), Vector(Vector(false, fa
                                                  //| lse, true, false, false), Vector(false, false, false, false, false)), Vecto
                                                  //| r(Vector(false, false, true, false, false), Vector(true, false, false, fals
                                                  //| e, false)), Vector(Vector(false, false, true, false, false), Vector(false, 
                                                  //| true, false, false, false)), Vector(Vector(false, false, true, false, false
                                                  //| ), Vector(false, false, true, false, false)), Vector(Vector(false, false, t
                                                  //| rue, false, false), Vector(false
                                                  //| Output exceeds cutoff limit.

 val randomStarts = (1 to 10).map{ x =>
  Iterator.fill(10*x)( (scala.util.Random.nextInt(11), scala.util.Random.nextInt(11)) ).toSet
 }                                                //> randomStarts  : scala.collection.immutable.IndexedSeq[scala.collection.immu
                                                  //| table.Set[(Int, Int)]] = Vector(Set((5,9), (5,7), (7,10), (1,3), (5,5), (4,
                                                  //| 8), (3,7), (10,4), (8,6)), Set((2,5), (10,3), (7,9), (8,10), (9,0), (8,9), 
                                                  //| (4,7), (5,9), (6,9), (4,9), (4,8), (8,4), (3,3), (1,7), (8,7), (6,0), (9,4)
                                                  //| , (7,2)), Set((7,9), (5,0), (3,9), (0,2), (0,0), (6,10), (6,9), (1,6), (0,5
                                                  //| ), (6,5), (4,6), (4,5), (0,8), (5,4), (2,2), (4,2), (0,1), (10,4), (9,6), (
                                                  //| 5,3), (3,3), (10,0), (2,1), (8,7), (1,0)), Set((7,9), (5,0), (10,5), (3,9),
                                                  //|  (0,2), (6,4), (2,10), (9,1), (5,9), (8,1), (2,0), (6,5), (1,1), (6,3), (3,
                                                  //| 5), (1,10), (4,6), (8,3), (10,7), (2,6), (10,2), (0,4), (5,7), (3,2), (7,10
                                                  //| ), (4,8), (4,2), (3,7), (10,0), (1,2), (2,1), (8,5), (7,2), (9,5)), Set((7,
                                                  //| 1), (1,8), (0,0), (3,4), (6,4), (7,7), (0,9), (6,6), (3,1), (6,1), (4,1), (
                                                  //| 5,9), (0,7), (2,0), (6,9), (8,0), (3,6), (6,5), (6,3), (7,3), (8,3), (10,7)
                                                  //| , (10,2), (8,2), (0,8), (8,8), (2,9), (3,2), (1,3), (4,2), (2,4), (10,4), (
                                                  //| 8,4), (5,8), (9,9), (6,0), (9,3), (0,6), (9,2), (7,0)), Set((1,8), (10,3), 
                                                  //| (9,10), (8,10), (10,5), (9,0), (6,7), (0,2), (0,0), (5,2), (5,1), (0,10), (
                                                  //| 4,7), (2,10), (3,1), (6,2), (8,1), (3,0), (8,0), (1,6), (0,5), (1,1), (4,6)
                                                  //| , (8,3), (10,7), (9,8), (1,9), (4,5), (1,4), (10,2), (0,8), (2,9), (5,4), (
                                                  //| 2,7), (10,9), (9,6), (8,4), (5,3), (5,8), (1,7), (2,3), (2,1), (4,3), (8,7)
                                                  //| , (7,2), (9,5), (3,8), (5,6), (0,6), (9,2), (7,0)), Set((7,5), (2,5), (1,5)
                                                  //| , (7,9), (9,10), (5,0), (8,10), (10,5), (6,7), (3,9), (0,2), (7,4), (5,1), 
                                                  //| (4,10), (3,4), (6,4), (0,10), (7,7), (2,10), (6,6), (7,8), (3,1), (6,1), (4
                                                  //| ,1), (5,9), (6,2), (0,7), (0,3), (3,0), (1,6), (6,8), (7,3), (1,10), (9,8),
                                                  //|  (1,9), (0,8), (8,8), (5,4), (10,6), (1,3), (2,2), (3,7), (10,4), (9,6), (3
                                                  //| ,3), (1,2), (4,3), (6,0), (8,5), (10,8), (3,8), (0,6)), Set((1,8), (2,5), (
                                                  //| 10,3), (1,5), (5,0), (8,10), (9,0), (6,7), (8,9), (0,2), (4,10), (3,4), (6,
                                                  //| 4), (0,10), (7,8), (3,1), (4,1), (5,9), (6,2), (8,1), (0,3), (6,9), (3,0), 
                                                  //| (1,6), (10,1), (3,6), (6,8), (1,1), (3,5), (7,3), (1,10), (10,7), (9,8), (4
                                                  //| ,5), (1,4), (8,2), (4,9), (0,4), (9,7), (7,10), (4,8), (4,2), (10,9), (3,3)
                                                  //| , (1,7), (2,3), (2,1), (4,3), (8,7), (6,0), (8,5), (7,2), (10,8), (8,6), (3
                                                  //| ,8), (5,6), (0,6)), Set((7,1), (7,5), (7,6), (1,5), (7,9), (0,0), (6,10), (
                                                  //| 7,4), (5,1), (4,0), (3,4), (7,7), (4,7), (2,10), (0,9), (6,6), (3,1), (4,1)
                                                  //| , (5,9), (2,0), (6,9), (8,0), (5,10), (10,10), (1,6), (0,5), (2,8), (6,8), 
                                                  //| (3,5), (4,6), (8,3), (10,7), (4,5), (1,4), (8,2), (0,8), (8,8), (4,9), (5,4
                                                  //| ), (3,2), (1,3), (5,5), (2,7), (4,2), (2,4), (3,7), (10,9), (0,1), (10,4), 
                                                  //| (9,6), (8,4), (2,3), (10,0), (2,1), (9,9), (8,7), (9,4), (7,2), (9,5), (1,0
                                                  //| ), (5,6), (0,6), (7,0)), Set((7,1), (7,5), (10,3), (7,9), (9,10), (9,0), (8
                                                  //| ,9), (3,9), (0,2), (0,0), (5,2), (7,4), (5,1), (4,10), (3,4), (0,10), (4,7)
                                                  //| , (6,6), (7,8), (3,1), (9,1), (6,2), (8,1), (6,9), (4,4), (5,10), (10,10), 
                                                  //| (6,8), (1,1), (6,3), (1,10), (10,7), (9,8), (1,9), (4,5), (2,6), (8,2), (8,
                                                  //| 8), (4,9), (0,4), (2,9), (5,7), (9,7), (5,5), (2,7), (4,2), (2,4), (3,7), (
                                                  //| 10,9), (0,1), (8,4), (5,3), (5,8), (3,3), (1,7), (10,0), (9,9), (6,0), (9,4
                                                  //| ), (8,6), (1,0), (3,8), (5,6), (0,6), (9,2)))
 
 /*val rulesWithSums = allRules.map{ rule =>
  (rule , randomStarts.map{ startTorus =>
   calcEnergy( findPeriod( startTorus, rule, 0))}.sum )
   }*/
 
 
 //rulesWithSums.sortBy(_._2).take(8)
 //rulesWithSums.sortBy(_._2).reverse.take(8)
 

 val solfromSiteRules = Vector(Vector( false, true,true, false, false), Vector( false, false, false, true, false))
                                                  //> solfromSiteRules  : scala.collection.immutable.Vector[scala.collection.immu
                                                  //| table.Vector[Boolean]] = Vector(Vector(false, true, true, false, false), Ve
                                                  //| ctor(false, false, false, true, false))
 val solfromSiteTorus = Set( (4,2), (3,3), (2,4), (2,5),(2,6),(3,7),(4,6),(5,7),(6,6),(6,5),(6,4),(5,3))
                                                  //> solfromSiteTorus  : scala.collection.immutable.Set[(Int, Int)] = Set((2,5),
                                                  //|  (6,4), (6,6), (6,5), (4,6), (2,6), (5,7), (4,2), (2,4), (3,7), (5,3), (3,3
                                                  //| ))


 printUniverse(solfromSiteTorus)                  //> ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ..*.*......
                                                  //| .*.*.*.....
                                                  //| .*...*.....
                                                  //| .*...*.....
                                                  //| ..*.*......
                                                  //| ...*.......
                                                  //| ...........
 findPeriod(solfromSiteTorus, solfromSiteRules, 0)//> Solution Found!!!
                                                  //| Rules Vector(Vector(false, true, true, false, false), Vector(false, false, 
                                                  //| false, true, false))
                                                  //| i 192713
                                                  //| res2: (Int, Int) = (72,192713)
 printUniverse(updateUniverse(solfromSiteTorus, solfromSiteRules))
                                                  //> ...........
                                                  //| ...........
                                                  //| ...........
                                                  //| ..*.*......
                                                  //| .*...*.....
                                                  //| *.....*....
                                                  //| *.***.*....
                                                  //| *.*.*.*....
                                                  //| .*...*.....
                                                  //| ..*.*......
                                                  //| ...*.......
 printUniverse(updateUniverse(updateUniverse(solfromSiteTorus, solfromSiteRules), solfromSiteRules))
                                                  //> ...*.......
                                                  //| ...........
                                                  //| ..*.*......
                                                  //| .*.*.*.....
                                                  //| *.*.*.*....
                                                  //| .*****.*..*
                                                  //| .*...*.*..*
                                                  //| .......*..*
                                                  //| *.....*....
                                                  //| .*...*.....
                                                  //| ..*.*......
                                                  

 
 
 def main(args: Array[String]) = {
  Iterator.from(0).find{it =>
  val x: Torus = Set((7,6), (9,10), (8,10), (10,5), (3,9), (0,2), (0,0), (5,1), (6,4), (0,10), (2,10),
   (6,6), (3,1), (6,2), (8,1), (1,6), (10,1), (2,8), (1,1), (6,3), (3,5), (10,7),
   (4,5), (0,8), (8,8), (0,4), (5,4), (7,10), (2,2), (2,7), (10,9), (1,2), (9,9), (8,5), (8,6))
                                                  //Set( (5,5))
  
  val r: Rules = Vector(
  Vector(false, true, false, false, false),
  Vector(false, false, true, false, false))
  
//  Vector( Vector( false, true, false, true, false), Vector( false, true, true, true, false))
 
  m.clear()
  //   findPeriod(x, r, 0) == 72
  val simResults = simulatedAnnealing( (x,r), 100.0)
  val boolResult = calcEnergy( findPeriod(simResults._1, simResults._2, 0))  == 0
  if (boolResult)
   println(simResults)
  boolResult
 }
 }                                                //> main: (args: Array[String])Option[Int]
 }