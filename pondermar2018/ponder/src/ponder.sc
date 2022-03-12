import scala.math.Ordering.Implicits._
object ponder {
 val vowels = 5 // 5 Number of vowels             //> vowels  : Int = 5
 val switches = 6 // 6 number of switches         //> switches  : Int = 6

  def allNodes(s: Int): List[List[Int]] = {
   s match {
    case 0 => List(List())
    case _ => (for {
     b <- 0 to 1
     rest <- allNodes(s-1)
    } yield b :: rest).toList
   }
  }                                               //> allNodes: (s: Int)List[List[Int]]
  
  val all3 = allNodes(switches)                   //> all3  : List[List[Int]] = List(List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 1
                                                  //| ), List(0, 0, 0, 0, 1, 0), List(0, 0, 0, 0, 1, 1), List(0, 0, 0, 1, 0, 0), L
                                                  //| ist(0, 0, 0, 1, 0, 1), List(0, 0, 0, 1, 1, 0), List(0, 0, 0, 1, 1, 1), List(
                                                  //| 0, 0, 1, 0, 0, 0), List(0, 0, 1, 0, 0, 1), List(0, 0, 1, 0, 1, 0), List(0, 0
                                                  //| , 1, 0, 1, 1), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 1, 0, 1), List(0, 0, 1,
                                                  //|  1, 1, 0), List(0, 0, 1, 1, 1, 1), List(0, 1, 0, 0, 0, 0), List(0, 1, 0, 0, 
                                                  //| 0, 1), List(0, 1, 0, 0, 1, 0), List(0, 1, 0, 0, 1, 1), List(0, 1, 0, 1, 0, 0
                                                  //| ), List(0, 1, 0, 1, 0, 1), List(0, 1, 0, 1, 1, 0), List(0, 1, 0, 1, 1, 1), L
                                                  //| ist(0, 1, 1, 0, 0, 0), List(0, 1, 1, 0, 0, 1), List(0, 1, 1, 0, 1, 0), List(
                                                  //| 0, 1, 1, 0, 1, 1), List(0, 1, 1, 1, 0, 0), List(0, 1, 1, 1, 0, 1), List(0, 1
                                                  //| , 1, 1, 1, 0), List(0, 1, 1, 1, 1, 1), List(1, 0, 0, 0, 0, 0), List(1, 0, 0,
                                                  //|  0, 0, 1), List(1, 0, 0, 0, 1, 0), List(1, 0, 0, 0, 1, 1), List(1, 0, 0, 1, 
                                                  //| 0, 0), List(1, 0, 0, 1, 0, 1), List(1, 0, 0, 1, 1, 0), List(1, 0, 0, 1, 1, 1
                                                  //| ), List(1, 0, 1, 0, 0, 0), List(1, 0, 1, 0, 0, 1), List(1, 0, 1, 0, 1, 0), L
                                                  //| ist(1, 0, 1, 0, 1, 1), List(1, 0, 1, 1, 0, 0), List(1, 0, 1, 1, 0, 1), List(
                                                  //| 1, 0, 1, 1, 1, 0), List(1, 0, 1, 1, 1, 1), List(1, 1, 0, 0, 0, 0), List(1, 1
                                                  //| , 0, 0, 0, 1), List(1, 1, 0, 0, 1, 0), List(1, 1, 0, 0, 1, 1), List(1, 1, 0,
                                                  //|  1, 0, 0), List(1, 1, 0, 1, 0, 1), List(1, 1, 0, 1, 1, 0), List(1, 1, 0, 1, 
                                                  //| 1, 1), List(1, 1, 1, 0, 0, 0), List(1, 1, 1, 0, 0, 1), List(1, 1, 1, 0, 1, 0
                                                  //| ), List(1, 1, 1, 0, 1, 1), List(1, 1, 1, 1, 0, 0), List(1, 1, 1, 1, 0, 1), L
                                                  //| ist(1, 1, 1, 1, 1, 0), List(1, 1, 1, 1, 1, 1))
  val all3withV = all3.map(x=> x ->  0) toMap     //> all3withV  : scala.collection.immutable.Map[List[Int],Int] = Map(List(0, 1, 
                                                  //| 1, 0, 0, 0) -> 0, List(1, 1, 0, 1, 0, 0) -> 0, List(1, 0, 0, 1, 1, 0) -> 0, 
                                                  //| List(1, 1, 0, 0, 1, 0) -> 0, List(1, 0, 0, 1, 0, 1) -> 0, List(0, 1, 0, 0, 1
                                                  //| , 1) -> 0, List(0, 0, 0, 1, 1, 1) -> 0, List(1, 1, 0, 1, 1, 1) -> 0, List(0,
                                                  //|  0, 0, 0, 0, 0) -> 0, List(1, 1, 1, 1, 1, 1) -> 0, List(0, 1, 0, 0, 0, 1) ->
                                                  //|  0, List(1, 0, 0, 1, 0, 0) -> 0, List(1, 0, 1, 1, 0, 1) -> 0, List(0, 1, 0, 
                                                  //| 1, 1, 1) -> 0, List(1, 0, 0, 0, 0, 0) -> 0, List(1, 0, 0, 1, 1, 1) -> 0, Lis
                                                  //| t(0, 0, 1, 0, 0, 0) -> 0, List(0, 1, 0, 1, 1, 0) -> 0, List(0, 0, 0, 0, 1, 0
                                                  //| ) -> 0, List(0, 0, 0, 1, 0, 1) -> 0, List(0, 0, 1, 0, 1, 1) -> 0, List(1, 0,
                                                  //|  0, 0, 1, 1) -> 0, List(1, 0, 1, 0, 0, 0) -> 0, List(0, 0, 1, 1, 0, 1) -> 0,
                                                  //|  List(1, 1, 1, 1, 1, 0) -> 0, List(0, 0, 1, 1, 1, 0) -> 0, List(0, 0, 1, 0, 
                                                  //| 1, 0) -> 0, List(0, 1, 0, 1, 0, 1) -> 0, List(0, 1, 0, 0, 0, 0) -> 0, List(0
                                                  //| , 1, 1, 1, 0, 1) -> 0, List(0, 1, 1, 0, 1, 0) -> 0, List(0, 0, 0, 1, 1, 0) -
                                                  //| > 0, List(1, 1, 1, 0, 0, 0) -> 0, List(0, 1, 1, 1, 1, 0) -> 0, List(1, 1, 1,
                                                  //|  0, 1, 0) -> 0, List(1, 1, 0, 0, 1, 1) -> 0, List(1, 1, 1, 1, 0, 0) -> 0, Li
                                                  //| st(0, 0, 1, 0, 0, 1) -> 0, List(1, 0, 1, 0, 0, 1) -> 0, List(1, 0, 1, 1, 1, 
                                                  //| 0) -> 0, List(0, 1, 1, 1, 0, 0) -> 0, List(1, 1, 1, 0, 1, 1) -> 0, List(1, 1
                                                  //| , 1, 1, 0, 1) -> 0, List(1, 0, 1, 0, 1, 1) -> 0, List(0, 0, 0, 0, 1, 1) -> 0
                                                  //| , List(1, 1, 1, 0, 0, 1) -> 0, List(1, 1, 0, 0, 0, 1) -> 0, List(0, 0, 1, 1,
                                                  //|  0, 0) -> 0, List(0, 1, 1, 1, 1, 1) -> 0, List(0, 1, 0, 1, 0, 0) -> 0, List(
                                                  //| 1, 0, 1, 1, 0, 0) -> 0, List(0, 1, 0, 0, 1, 0) -> 0, List(1, 0, 1, 1, 1, 1) 
                                                  //| -> 0, List(0, 1, 1, 0, 0, 1) -> 0, List(1, 0, 1, 0, 1, 0) -> 0, List(1, 1, 0
                                                  //| , 1, 0, 1) -> 0, List(0, 0, 0, 1, 0, 0) -> 0, List(1, 1, 0, 1, 1, 0) -> 0, L
                                                  //| ist(1, 1, 0, 0, 0, 0) -> 0, List(0, 0, 1, 1, 1, 1) -> 0, List(1, 0, 0, 0, 0,
                                                  //|  1) -> 0, List(0, 0, 0, 0, 0, 1) -> 0, List(0, 1, 1, 0, 1, 1) -> 0, List(1, 
                                                  //| 0, 0, 0, 1, 0) -> 0)
  
 def allNeighbours(n: List[Int]) = {
  n.zipWithIndex.map{ case(elem,index) => n.updated(index, 1-elem)}
 }                                                //> allNeighbours: (n: List[Int])List[List[Int]]
 
 allNeighbours(List(0,1,0))                       //> res0: List[List[Int]] = List(List(1, 1, 0), List(0, 0, 0), List(0, 1, 1))

 allNeighbours(List(0,1,0)).map { x => all3withV.getOrElse(x, 0) }
                                                  //> res1: List[Int] = List(0, 0, 0)


// How many vowels we're missing > how many empty neighbours there are
 def notPossible(n: List[Int], all: Map[List[Int],Int]): Boolean = (1 to vowels).count{ y =>
  !(allNeighbours(n).exists{ all.getOrElse(_, 0) == y })} >
   1+ allNeighbours(n).count{ all.getOrElse(_, 0)  == 0 }
                                                  //> notPossible: (n: List[Int], all: Map[List[Int],Int])Boolean
  //  allNeighbours(n).map { all.getOrElse(_, 0) }.count{ _ == 0 }
def candidates (n: List[Int], all: Map[List[Int],Int]) = {
// There are two cases
// 1. There is already a duplicate amoungst the neighbours
// 2. Otherwise
// We cannot afford two duplicates.
// In case 1, we subtract all the neighbours form.
val neigh = allNeighbours(n).map{ all.getOrElse(_, 0)}
val neighboursWithCounts = neigh.groupBy(identity).mapValues(_.size)
val numDups = neighboursWithCounts.count(_._2 > 1)
 if (numDups >= 1)
  (1 to vowels) diff neigh
 else
  (1 to vowels) // We can allow anything
 }                                                //> candidates: (n: List[Int], all: Map[List[Int],Int])scala.collection.immutab
                                                  //| le.IndexedSeq[Int]

candidates(List(0,1,0), all3withV)                //> res2: scala.collection.immutable.IndexedSeq[Int] = Vector(1, 2, 3, 4, 5)



candidates(List(0,1,0), all3withV.updated(List(0,0,0), 1).updated(List(0,1,1), 1))
                                                  //> res3: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 3, 4, 5)
                                                  
def findSolution(nodes: Map[List[Int],Int]): Boolean =
{
 nodes.count{case (k,va) => va==0 } match {
  case 0 => {// success, print map
   nodes.toSeq.sortBy(_._1).map(println)
   // Not sure we're successful. Let's check
   // Can we reach all (1 to vowels) from every node.
   // Or to say it another way, does this node n, plus all neighbours of n, cover (1 to vowels)
   // Which is equal to (1 to vowels) diff (n+neighbours of n) == List() the empty list
   nodes.forall{ n =>
    val neigh = n._2 :: allNeighbours(n._1).map{ nodes.getOrElse(_, 0)}
    ((1 to vowels) diff neigh).size == 0
    }
  }
  case _ => {
//  println("nodes")
//   nodes.toSeq.sortBy(_._1).map(println)
   val remaining = nodes.filter{case (k,va) => va==0}
//  println("remaining")
// remaining.toSeq.sortBy(_._1).map(println)
   val bestnode = remaining.map( x =>(x._1, candidates(x._1, nodes ))).minBy(_._2.size)
 //  println(bestnode)
   // The exists in the next line should return false if the list is empty, without even descending into infinite recursion
   (
   !notPossible(bestnode._1, nodes) &&
   bestnode._2.exists{x => findSolution(nodes.updated( bestnode._1, x ) ) }
   )
  }
 }
}                                                 //> findSolution: (nodes: Map[List[Int],Int])Boolean/

findSolution(all3withV)
}