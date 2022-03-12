object ponder {
 val n = 11                                       //> n  : Int = 11
 def ijToIndex(i: Int, j: Int): Int =
  n*(n-1)/2 - (n-i)*(n-i-1)/2 + j-i-1             //> ijToIndex: (i: Int, j: Int)Int
 
 for{i<-0 until n
 j<- 0 until n}
 yield ijToIndex(i, j)                            //> res0: scala.collection.immutable.IndexedSeq[Int] = Vector(-1, 0, 1, 2, 3, 4,
                                                  //|  5, 6, 7, 8, 9, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 16, 17, 18, 19, 20
                                                  //| , 21, 22, 23, 24, 25, 26, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 29, 30
                                                  //| , 31, 32, 33, 34, 35, 36, 37, 38, 39, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43
                                                  //| , 44, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 41, 42, 43, 44, 45, 46, 47
                                                  //| , 48, 49, 50, 51, 43, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 44, 45, 46, 47
                                                  //| , 48, 49, 50, 51, 52, 53, 54, 44, 45, 46, 47, 48, 49, 50, 51, 52, 53, 54)
 // This looks good. Tested fine with n=5 too.
 
 def matFromIndex(m: Vector[Int], i: Int, j: Int): Int =
  if (i > j)
   -matFromIndex(m,j, i)
  else if (i == j)
   0
  else
   m(ijToIndex(i, j))                             //> matFromIndex: (m: Vector[Int], i: Int, j: Int)Int
  
 (0 until n).map{
 i =>
 (0 until n).map{ j =>
  matFromIndex((0 until n*n).toVector,i, j)
  } }                                             //> res1: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Index
                                                  //| edSeq[Int]] = Vector(Vector(0, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9), Vector(0, 0, 1
                                                  //| 0, 11, 12, 13, 14, 15, 16, 17, 18), Vector(-1, -10, 0, 19, 20, 21, 22, 23, 2
                                                  //| 4, 25, 26), Vector(-2, -11, -19, 0, 27, 28, 29, 30, 31, 32, 33), Vector(-3, 
                                                  //| -12, -20, -27, 0, 34, 35, 36, 37, 38, 39), Vector(-4, -13, -21, -28, -34, 0,
                                                  //|  40, 41, 42, 43, 44), Vector(-5, -14, -22, -29, -35, -40, 0, 45, 46, 47, 48)
                                                  //| , Vector(-6, -15, -23, -30, -36, -41, -45, 0, 49, 50, 51), Vector(-7, -16, -
                                                  //| 24, -31, -37, -42, -46, -49, 0, 52, 53), Vector(-8, -17, -25, -32, -38, -43,
                                                  //|  -47, -50, -52, 0, 54), Vector(-9, -18, -26, -33, -39, -44, -48, -51, -53, -
                                                  //| 54, 0))
  // This test passes too.
 
 // Can we insert the entry into position (i,j)?
 def canTake(mat: Vector[Int], entry: Int, i : Int, j: Int): Boolean =
  ((0 until j).map( jIndex => matFromIndex(mat, i, jIndex)).sum + entry).abs <= n - 1 - j &&
  ((0 until i).map( iIndex => matFromIndex(mat, iIndex, j)).sum + entry).abs <= n - 1 - i
                                                  //> canTake: (mat: Vector[Int], entry: Int, i: Int, j: Int)Boolean
  
 canTake( Vector(1,-1), -1, 1,2)                  //> res2: Boolean = true
 canTake( Vector(1,-1), 1, 1,2)                   //> res3: Boolean = true
 // The insertion of the last element in a 3x3 matrix (standard RPS) test pass.
 
 canTake( Vector(1), 1, 0,2)                      //> res4: Boolean = true
 canTake( Vector(1), -1,0,2)                      //> res5: Boolean = true
 
 // should crash and does
// canTake( Vector(1), 1,1,2)
//Vector(1)(1)
 (n*n-n)/2                                        //> res6: Int = 55
 def generateMatrices(matSoFar: Vector[Int], i: Int, j: Int): Iterator[Vector[Int]] = {
//  println("mat so far" + matSoFar)
//    println("i " + i + " j " + j)
  
  if (matSoFar.size == (n*n - n )/2)
   Iterator(matSoFar)
  else
   for { entry <- Iterator(1,-1)
    if (canTake(matSoFar, entry, i,j))
    rest <- generateMatrices(matSoFar :+ entry, if (j == n -1) i+1 else i, if (j== n-1) i+2 else j+1)
   } yield rest
 }                                                //> generateMatrices: (matSoFar: Vector[Int], i: Int, j: Int)Iterator[Vector[In
                                                  //| t]]
 //generateMatrices(Vector(), 0, 1).foreach(println)
 //generateMatrices(Vector(1), 0, 2).take(10).foreach(println)
 
 def generatePermutationAndCheckForIsomorphism(mat: Vector[Int], permSoFar: Vector[Int], remaining: Set[Int]): Iterator[Vector[Int]] =
 {
  if (remaining.isEmpty)
   Iterator( permSoFar )
  else
   for { entry <- remaining.toIterator
       if (permSoFar.zipWithIndex.forall{ case(p,i) => matFromIndex(mat, p, entry) == matFromIndex(mat, i, permSoFar.size)} )
       rest <- generatePermutationAndCheckForIsomorphism(mat, permSoFar :+ entry, remaining - entry)
   } yield rest
 }                                                //> generatePermutationAndCheckForIsomorphism: (mat: Vector[Int], permSoFar: Ve
                                                  //| ctor[Int], remaining: Set[Int])Iterator[Vector[Int]]

def printMat(m: Vector[Int])=
{
 println(m)

 (0 until n).map{ i =>
 (0 until n).map{ j =>
  matFromIndex(m,i, j).toString
  }.mkString(" ")
   }.foreach(println)
   
  (0 until n).map{ i=>
  println(i + " -> " + (0 until n).filter( j => matFromIndex(m, i, j) == 1))
  }
   
}                                                 //> printMat: (m: Vector[Int])scala.collection.immutable.IndexedSeq[Unit]
 val out = generateMatrices(Vector(), 0, 1).filter{ m => //println("m " + m)
 val isos = generatePermutationAndCheckForIsomorphism(m, Vector(), (0 until n).toSet)
 val isoList = isos.toList
 //println("num isos " + isoSize)
 val isSolution = isoList.size >= 50 // 10
 
 if (isSolution)
 {
  println("solution ")
  printMat(m)
  println("with isos ")
  isoList.foreach(println)
  }
  isSolution
 //isos.foreach(println)
 }//.foreach(println)                             //> solution 
                                                  //| Vector(1, 1, 1, 1, 1, -1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, -1, 1,
                                                  //|  1, -1, 1, -1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 1, 1, 1, -1, -1, 1, -1, -1,
                                                  //|  1, 1, 1, -1, 1, -1, 1, -1, 1, -1, -1, 1, -1)
                                                  //| 0 1 1 1 1 1 -1 -1 -1 -1 -1
                                                  //| -1 0 1 1 -1 -1 1 1 1 -1 -1
                                                  //| -1 -1 0 1 1 -1 1 -1 -1 1 1
                                                  //| -1 -1 -1 0 1 1 -1 1 1 1 -1
                                                  //| -1 1 -1 -1 0 1 1 1 -1 -1 1
                                                  //| -1 1 1 -1 -1 0 -1 -1 1 1 1
                                                  //| 1 -1 -1 1 -1 1 0 -1 1 -1 1
                                                  //| 1 -1 1 -1 -1 1 1 0 -1 1 -1
                                                  //| 1 -1 1 -1 1 -1 -1 1 0 -1 1
                                                  //| 1 1 -1 -1 1 -1 1 -1 1 0 -1
                                                  //| 1 1 -1 1 -1 -1 -1 1 -1 1 0
                                                  //| 0 -> Vector(1, 2, 3, 4, 5)
                                                  //| 1 -> Vector(2, 3, 6, 7, 8)
                                                  //| 2 -> Vector(3, 4, 6, 9, 10)
                                                  //| 3 -> Vector(4, 5, 7, 8, 9)
                                                  //| 4 -> Vector(1, 5, 6, 7, 10)
                                                  //| 5 -> Vector(1, 2, 8, 9, 10)
                                                  //| 6 -> Vector(0, 3, 5, 8, 10)
                                                  //| 7 -> Vector(0, 2, 5, 6, 9)
                                                  //| 8 -> Vector(0, 2, 4, 7, 10)
                                                  //| 9 -> Vector(0, 1, 4, 6, 8)
                                                  //| 10 -> Vector(0, 1, 3, 7, 9)
                                                  //| with isos 
                                                  //| Vector(0, 5, 1, 2, 3, 4, 8, 9, 10, 6, 7)
  
 out.take(2).foreach(println)                     //> Vector(1, 1, 1, 1, 1, -1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, -1, 1,
                                                  //|  1, -1, 1, -1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 1, 1, 1, -1, -1, 1, -1, -1,
                                                  //|  1, 1, 1, -1, 1, -1, 1, -1, 1, -1, -1, 1, -1)
                                                  //| solution 
                                                  //| Vector(1, 1, 1, 1, 1, -1, -1, -1, -1, -1, 1, 1, -1, -1, 1, 1, 1, -1, -1, 1,
                                                  //|  1, -1, 1, -1, -1, 1, 1, 1, 1, -1, 1, 1, 1, -1, 1, 1, -1, 1, -1, 1, -1, 1, 
                                                  //| -1, 1, 1, 1, -1, -1, 1, 1, -1, 1, 1, -1, -1)
                                                  //| 0 1 1 1 1 1 -1 -1 -1 -1 -1
                                                  //| -1 0 1 1 -1 -1 1 1 1 -1 -1
                                                  //| -1 -1 0 1 1 -1 1 -1 -1 1 1
                                                  //| -1 -1 -1 0 1 1 -1 1 1 1 -1
                                                  //| -1 1 -1 -1 0 1 1 -1 1 -1 1
                                                  //| -1 1 1 -1 -1 0 -1 1 -1 1 1
                                                  //| 1 -1 -1 1 -1 1 0 1 -1 -1 1
                                                  //| 1 -1 1 -1 1 -1 -1 0 1 -1 1
                                                  //| 1 -1 1 -1 -1 1 1 -1 0 1 -1
                                                  //| 1 1 -1 -1 1 -1 1 1 -1 0 -1
                                                  //| 1 1 -1 1 -1 -1 -1 -1 1 1 0
                                                  //| 0 -> Vector(1, 2, 3, 4, 5)
                                                  //| 1 -> Vector(2, 3, 6, 7, 8)
                                                  //| 2 -> Vector(3, 4, 6, 9, 10)
                                                  //| 3 -> Vector(4, 5, 7, 8, 9)
                                                  //| 4 -> Vector(1, 5, 6, 8, 10)
                                                  //| 5 -> Vector(1, 2, 7, 9, 10)
                                                  //| 6 -> Vec
                                                  //| Output exceeds cutoff limit.
 
}