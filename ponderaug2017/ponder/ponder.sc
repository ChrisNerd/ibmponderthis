object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  /*
  4x6 inclusive
  so the dimentions are 5x7
  but not allowed to put the black balls on the rails, so that leaves only 3x5 = 15 spots.
  So a total of 8 black balls in 15 spots. 15 choose 8 = 6435, not worrying about the double
  counting due to symmetry.
  Each case needs a white ball somewhere in the 7 remaining spots.
  And to test each case, an upper bound is to go through the 8!=40320 ways of ordering them.
  
  So 6435*40320=259,459,200.
  What about
  B.B.B
  .B.B.
  B.B.B
  ?
  
  How do we see if a ball is capable of being sunk? Reflect the board about its top and right walls,
  and again to get 4 mirror tables.
  
  mirror mirror
  .....
  ....b mirror
  .....
  
  Now we're looking at the path the balls take. Take the delta x and delta y between the cue ball
  and the black ball. If they are relatively prime they don't pass on any unexpected grid points.
  Otherwise they pass on the gcd.
  Keep adding the deltax and deltay to the black ball (mod (10,14)) until it ends on a corner or another
  black ball or back where it started with the same velocity vector. Due to the pingeon hole
  eventually one of these case will be reached.
    
  */
  
  val a = for{
  x <- -5 to 5
  y <- -5 to 5
  } yield ((x,y))                                 //> a  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((-5,-5), (-
                                                  //| 5,-4), (-5,-3), (-5,-2), (-5,-1), (-5,0), (-5,1), (-5,2), (-5,3), (-5,4), (
                                                  //| -5,5), (-4,-5), (-4,-4), (-4,-3), (-4,-2), (-4,-1), (-4,0), (-4,1), (-4,2),
                                                  //|  (-4,3), (-4,4), (-4,5), (-3,-5), (-3,-4), (-3,-3), (-3,-2), (-3,-1), (-3,0
                                                  //| ), (-3,1), (-3,2), (-3,3), (-3,4), (-3,5), (-2,-5), (-2,-4), (-2,-3), (-2,-
                                                  //| 2), (-2,-1), (-2,0), (-2,1), (-2,2), (-2,3), (-2,4), (-2,5), (-1,-5), (-1,-
                                                  //| 4), (-1,-3), (-1,-2), (-1,-1), (-1,0), (-1,1), (-1,2), (-1,3), (-1,4), (-1,
                                                  //| 5), (0,-5), (0,-4), (0,-3), (0,-2), (0,-1), (0,0), (0,1), (0,2), (0,3), (0,
                                                  //| 4), (0,5), (1,-5), (1,-4), (1,-3), (1,-2), (1,-1), (1,0), (1,1), (1,2), (1,
                                                  //| 3), (1,4), (1,5), (2,-5), (2,-4), (2,-3), (2,-2), (2,-1), (2,0), (2,1), (2,
                                                  //| 2), (2,3), (2,4), (2,5), (3,-5), (3,-4), (3,-3), (3,-2), (3,-1), (3,0), (3,
                                                  //| 1), (3,2), (3,3), (3,4), (3,5), (4,-5), (4,-4), (4,-3), (4,-2), (4,-1), (4,
                                                  //| 0), (4,1), (4,2), (4,3)
                                                  //| Output exceeds cutoff limit.
  
  val b = a.sortBy( x => x._1 * x._1 + x._2 * x._2 )
                                                  //> b  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((0,0), (-1,
                                                  //| 0), (0,-1), (0,1), (1,0), (-1,-1), (-1,1), (1,-1), (1,1), (-2,0), (0,-2), (
                                                  //| 0,2), (2,0), (-2,-1), (-2,1), (-1,-2), (-1,2), (1,-2), (1,2), (2,-1), (2,1)
                                                  //| , (-2,-2), (-2,2), (2,-2), (2,2), (-3,0), (0,-3), (0,3), (3,0), (-3,-1), (-
                                                  //| 3,1), (-1,-3), (-1,3), (1,-3), (1,3), (3,-1), (3,1), (-3,-2), (-3,2), (-2,-
                                                  //| 3), (-2,3), (2,-3), (2,3), (3,-2), (3,2), (-4,0), (0,-4), (0,4), (4,0), (-4
                                                  //| ,-1), (-4,1), (-1,-4), (-1,4), (1,-4), (1,4), (4,-1), (4,1), (-3,-3), (-3,3
                                                  //| ), (3,-3), (3,3), (-4,-2), (-4,2), (-2,-4), (-2,4), (2,-4), (2,4), (4,-2), 
                                                  //| (4,2), (-5,0), (-4,-3), (-4,3), (-3,-4), (-3,4), (0,-5), (0,5), (3,-4), (3,
                                                  //| 4), (4,-3), (4,3), (5,0), (-5,-1), (-5,1), (-1,-5), (-1,5), (1,-5), (1,5), 
                                                  //| (5,-1), (5,1), (-5,-2), (-5,2), (-2,-5), (-2,5), (2,-5), (2,5), (5,-2), (5,
                                                  //| 2), (-4,-4), (-4,4), (4,-4), (4,4), (-5,-3), (-5,3), (-3,-5), (-3,5), (3,-5
                                                  //| ), (3,5), (5,-3), (5,3)
                                                  //| Output exceeds cutoff limit.
  
  3.toLong                                        //> res0: Long = 3

val digits = 15                                   //> digits  : Int = 15

def gcd(a: Int, b: Int): Int = if (b == 0) a else gcd(b, a % b)
                                                  //> gcd: (a: Int, b: Int)Int

val g = gcd(10,15)                                //> g  : Int = 5

Stream.from(g, g).find { x => x > 18 }            //> res1: Option[Int] = Some(20)

  val ns = for { i <- 1 to Math.pow(2, 4).toInt - 5
  } yield String.format("%" + digits + "s", i.toBinaryString).replace(' ', '0').toList
                                                  //> ns  : scala.collection.immutable.IndexedSeq[List[Char]] = Vector(List(0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 1, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1), List(0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 1, 0, 1), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 0), List
                                                  //| (0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 1, 0, 0, 0), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 1), L
                                                  //| ist(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 0, 1, 0), List(0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 1, 0, 1, 1))
 
 val n = ns.head                                  //> n  : List[Char] = List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1)
 val n2 =ns.filter{ _.count{ x => x == '1' } == 3}//> n2  : scala.collection.immutable.IndexedSeq[List[Char]] = Vector(List(0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1), List(0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 1, 0, 1, 1))
 
 n.zipWithIndex.filter(p => p._1 == '0')          //> res2: List[(Char, Int)] = List((0,0), (0,1), (0,2), (0,3), (0,4), (0,5), (0
                                                  //| ,6), (0,7), (0,8), (0,9), (0,10), (0,11), (0,12), (0,13))
 n.zipWithIndex.filter(p => p._1 == '0').map(_._2)//> res3: List[Int] = List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)

 
 val arrCollection = n2.map{_.grouped(5).toList}  //> arrCollection  : scala.collection.immutable.IndexedSeq[List[List[Char]]] = 
                                                  //| Vector(List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 1, 1, 1)),
                                                  //|  List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 1, 0, 1, 1)))
 
 //arr(2)(3)
 val arr = arrCollection.head                     //> arr  : List[List[Char]] = List(List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), Li
                                                  //| st(0, 0, 1, 1, 1))
 
 for { rowindex <- arr.indices
 charindex <- arr(rowindex).indices
 if arr(rowindex)(charindex) == '1'
 } yield arr.updated(rowindex, arr(rowindex).updated(charindex, 'w'))
                                                  //> res4: scala.collection.immutable.IndexedSeq[List[List[Char]]] = Vector(List
                                                  //| (List(0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, w, 1, 1)), List(List(
                                                  //| 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 1, w, 1)), List(List(0, 0, 
                                                  //| 0, 0, 0), List(0, 0, 0, 0, 0), List(0, 0, 1, 1, w)))
 
 // We actually don't need to do the updated command on the line above, we just need to return the indices
val whitespaces =  for { rowindex <- arr.indices
 charindex <- arr(rowindex).indices
 if arr(rowindex)(charindex) == '1'
 } yield (rowindex,charindex)                     //> whitespaces  : scala.collection.immutable.IndexedSeq[(Int, Int)] = Vector((
                                                  //| 2,2), (2,3), (2,4))
 
val wf = whitespaces.find{ w => w == (2,30) }     //> wf  : Option[(Int, Int)] = None


val wf2 = wf match {
case Some(_) => 23
case None => 24
}                                                 //> wf2  : Int = 24
 
 
 
  import scala.collection.immutable.Queue

def breadth_first_traverse[Node](node: Node, f: Node => Queue[Node]): Stream[Node] = {
  def recurse(q: Queue[Node]): Stream[Node] = {
    if (q.isEmpty) {
      Stream.Empty
    } else {
      val (node, tail) = q.dequeue
      node #:: recurse(tail ++ f(node))
    }
  }

  node #:: recurse(Queue.empty ++ f(node))
}                                                 //> breadth_first_traverse: [Node](node: Node, f: Node => scala.collection.immu
                                                  //| table.Queue[Node])Stream[Node]
  
  
  
  
}