object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  def allowedPositions(n: Int, a: List[Int], N: Int): List[Int] =
  {
   val apairs = (a zip (a drop(n+1))).zipWithIndex
  val cands = apairs filter { case (itemPair, itemIndex) => itemPair._1 == 0 && itemPair._2 == 0 } map (_._2)
  cands filter { x=>
   {
    if (n==N)
    true
    else
{
    val c = a.updated(x, n).updated(x+n+1,n)
    (n+1 to N) forall {highern => canWork(highern, c) }
 }
   }
   }
  }                                               //> allowedPositions: (n: Int, a: List[Int], N: Int)List[Int]
  
   def canWork(n: Int, a: List[Int]): Boolean =
  {
   (a zip (a drop(n+1))).zipWithIndex exists { case (itemPair, itemIndex) => itemPair._1 == 0 && itemPair._2 == 0 }
  }                                               //> canWork: (n: Int, a: List[Int])Boolean
  
  
  val a = List(0,1,0,0,0)                         //> a  : List[Int] = List(0, 1, 0, 0, 0)
  a zip (a drop(1+1))                             //> res0: List[(Int, Int)] = List((0,0), (1,0), (0,0))
  allowedPositions(1, a,3)                        //> res1: List[Int] = List()
  
  
  // Given a List a, and an int n, try all allowed position until one works
  def findSolution(n: Int, a: List[Int], N: Int): Boolean =
  {
/*  if (n < 10)
  println(a)
  */
  if (n == N+1)
   {
    println(a)
    true //	    Some(a)
   }
  else
  {
   val aP = allowedPositions(n, a,N)
   val cands = aP map {x=> a.updated(x, n).updated(x+n+1,n)}
   val candsRan = scala.util.Random.shuffle(cands)
   candsRan.exists(x => findSolution(n+1,x,N) )
  }
 }                                                //> findSolution: (n: Int, a: List[Int], N: Int)Boolean
 val N=6                                          //> N  : Int = 6
 findSolution(1, List.fill(N)(0) ::: -1 :: List.fill(N)(0), N)
                                                  //> res2: Boolean = false
 
 
 
 def main(args:Array[String]) = {
 if (!args.isEmpty){
 val N = args(0).toInt
 scala.util.Random.shuffle(0 to N) map (x=> findSolution(1, List.fill(x)(0) ::: -1 :: List.fill(2*N-x)(0),N))
 //(26 to 26) map (x=> findSolution(1, List.fill(x)(0) ::: -1 :: List.fill(2*N-x)(0),N))
}
}                                                 //> main: (args: Array[String])Any
 
 
// findSolution(1, List.fill(2*N)(0))
  
  
  /*
  N=4
   List(4, 1, 3, 1, 2, 4, 3, 2)
N=5 false with no hyphen
N=6 false with no hyphen
         List(1, 7, 1, 2, 5, 6, 2, 3, 4, 7, 5, 3, 6, 4)
                                                  
                                                                           
                                                  
                                                  */
  
  
}