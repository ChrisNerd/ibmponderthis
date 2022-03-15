object ponder {
/*
s=2
r=0
for b in v:
    if (s modulo 7) = b:
        r = r+2*b-1
    s = 6+s*(5+s*s*s*(2+s*(3+s))) + b*(5+s*(5+s*(6+s*s*(3+s*6))))
return r
*/

// Transform this into a tree, where nodes are the s values, and the edges to the two children represent b=0 and b=1.
// Root node is when s=2.
// r is calculated as the distribution of values of r over all the leaf nodes below it. That way we can
// cache this r distribution and make a super efficient algorithm.
// rDistribution = distributionFromLeftChild + {-1,0,1} depending on s mod 7 == b.
//                + ... rightChild .....
// If we're at a leaf node, r is just a delta function at that value.

// By caching our r distributions we need at most 14 distributions per depth of the tree. With a max depth of 14, we are below
// 14*42 = 588 caches. This algorithm should be instantaneous!
val rCache = collection.mutable.Map[(Int, Int, Int), Map[Int, BigInt]]()
                                                  //> rCache  : scala.collection.mutable.Map[(Int, Int, Int),Map[Int,BigInt]] = Ma
                                                  //| p()
// Need a function that maps s to s, as a function of b.
// s=f(s,b)
def sFunc(s: Int, b: Int): Int = {
 (6+s*(5+s*s*s*(2+s*(3+s))) + b*(5+s*(5+s*(6+s*s*(3+s*6)))))% 7
}                                                 //> sFunc: (s: Int, b: Int)Int
 
sFunc(0,0)                                        //> res0: Int = 6
sFunc(1,0)                                        //> res1: Int = 3
sFunc(2,0)                                        //> res2: Int = 5
sFunc(3,0)                                        //> res3: Int = 3
sFunc(4,0)                                        //> res4: Int = 6
sFunc(5,0)                                        //> res5: Int = 3
sFunc(6,0)                                        //> res6: Int = 1
sFunc(0,1)                                        //> res7: Int = 4
sFunc(1,1)                                        //> res8: Int = 0
sFunc(2,1)                                        //> res9: Int = 4
sFunc(3,1)                                        //> res10: Int = 0
sFunc(4,1)                                        //> res11: Int = 4
sFunc(5,1)                                        //> res12: Int = 4
sFunc(6,1)                                        //> res13: Int = 4

// let N = |v| = 6 or 42
// The root node is at depth=1, where s=2
def rDist(depth: Int, s: Int, N: Int): Map[Int, BigInt] = {
	if (depth == N)
	{
	  // When we're at a leaf node, there are still two cases for r, i.e. when b=0 and b=1.
	  // You might think that we don't have to worry about when r is unchanged, i.e. 0 -> x, but we still have to count those cases.
    s match {
     case 0 => Map(-1 -> 1, 0 -> 1)  // one count of r decreasing by 1, and one count of r remaining unchanged.
     case 1 => Map(1 -> 1, 0 -> 1)
	   case _ => Map(0 -> 2)
	  }
	}
	else
	{
		// r0 is the r distribution for the b=0 child node
		val r0 = rCache.getOrElseUpdate((depth+1, sFunc(s,0), N), rDist(depth+1, sFunc(s,0), N)) // rDist(depth+1, sFunc(s,0))
		val r1 = rCache.getOrElseUpdate((depth+1, sFunc(s,1), N), rDist(depth+1, sFunc(s,1), N)) // rDist(depth+1, sFunc(s,1))
		
		// When s==0, all the b=0 child nodes need to have their r distribution shifted by -1 (We are convolving with a delta function at -1).
		// when s==1, all the b=1 child nodes need to have their r distribution shifted by +1
		// then we sum the two (possibly shifted) child r distributions.
		// There's actually a sweet way of summing two maps combining keys using semigroups in scalaz.
    s match {
     case 0 => {
      val r0shifted = r0.map{ case (k,v) => (k-1) -> v}
      r0shifted ++ r1.map{ case (k,v) => k -> (v + r0shifted.getOrElse(k,0)) }
     }
     case 1 =>
     {
      val r1shifted = r1.map{ case (k,v) => (k+1) -> v}
      r0 ++ r1shifted.map{ case (k,v) => k -> (v + r0.getOrElse(k,0)) }
     }
     case _ => r0 ++ r1.map{ case (k,v) => k -> (v + r0.getOrElse(k,0)) }
   }
  }
}                                                 //> rDist: (depth: Int, s: Int, N: Int)Map[Int,BigInt]

//Ahh! Negative counts. Switching to BigIntegers
val solution6 = rDist(1, 2, 6)                    //> solution6  : Map[Int,BigInt] = Map(0 -> 50, -1 -> 7, 1 -> 7)
solution6.values.sum                              //> res14: BigInt = 64
BigInt(2).pow(6)                                  //> res15: scala.math.BigInt = 64
solution6.filterKeys { x => x!=0 }.values.sum     //> res16: BigInt = 14


val solution42 = rDist(1, 2, 42)                  //> solution42  : Map[Int,BigInt] = Map(0 -> 1126394145840, 5 -> 7531634712, 10
                                                  //|  -> 88, -7 -> 55713662, -8 -> 2010376, -3 -> 179440588408, 1 -> 91190805850
                                                  //| 0, 6 -> 828807992, -4 -> 44728244960, 9 -> 29806, 2 -> 491331094128, -5 -> 
                                                  //| 7531634712, -10 -> 88, 7 -> 55713662, 3 -> 179440588408, -1 -> 911908058500
                                                  //| , -9 -> 29806, 8 -> 2010376, -6 -> 828807992, 4 -> 44728244960, -2 -> 49133
                                                  //| 1094128)
solution42.values.sum                             //> res17: BigInt = 4398046511104
BigInt(2).pow(42)                                 //> res18: scala.math.BigInt = 4398046511104
solution42.filterKeys { x => x!=0 }.values.sum    //> res19: BigInt = 3271652365264
// Answer is 3271652365264
val solution14 = rDist(1, 2, 14)                  //> solution14  : Map[Int,BigInt] = Map(0 -> 8088, -3 -> 18, 1 -> 3598, 2 -> 53
                                                  //| 2, 3 -> 18, -1 -> 3598, -2 -> 532)
solution14.values.sum                             //> res20: BigInt = 16384
BigInt(2).pow(14)                                 //> res21: scala.math.BigInt = 16384
solution14.filterKeys { x => x!=0 }.values.sum    //> res22: BigInt = 8296
}