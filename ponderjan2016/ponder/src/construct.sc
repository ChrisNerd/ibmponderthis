object construct {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet

  def factors(n: Int): List[(Int, Int)] =
    {
      (1 to n).toList filter (x => x * Math.floor(n / x) == n) map (x => (x, n / x))
    }                                             //> factors: (n: Int)List[(Int, Int)]

  factors(6)                                      //> res0: List[(Int, Int)] = List((1,6), (2,3), (3,2), (6,1))

  def covers(n: Int): List[(Int, Int)] =
    {
      if (n > 1)
        factors(n - 1) ::: factors(n) ::: factors(n + 1)
      else
        factors(n) ::: factors(n + 1)
    }                                             //> covers: (n: Int)List[(Int, Int)]

  covers(3)                                       //> res1: List[(Int, Int)] = List((1,2), (2,1), (1,3), (3,1), (1,4), (2,2), (4,1
                                                  //| ))

  (1 to 4) map factors                            //> res2: scala.collection.immutable.IndexedSeq[List[(Int, Int)]] = Vector(List(
                                                  //| (1,1)), List((1,2), (2,1)), List((1,3), (3,1)), List((1,4), (2,2), (4,1)))
  (1 to 4) map covers                             //> res3: scala.collection.immutable.IndexedSeq[List[(Int, Int)]] = Vector(List(
                                                  //| (1,1), (1,2), (2,1)), List((1,1), (1,2), (2,1), (1,3), (3,1)), List((1,2), (
                                                  //| 2,1), (1,3), (3,1), (1,4), (2,2), (4,1)), List((1,3), (3,1), (1,4), (2,2), (
                                                  //| 4,1), (1,5), (5,1)))

  def r(c: IndexedSeq[List[(Int, Int)]], S1: Set[Int], S2: Set[Int]): Boolean = {
    if (S1.size > 6 || S2.size > 6)
      false
    else if (c isEmpty) {
      println("Solution found!")
      println(S1.toList.sorted)
      println(S2.toList.sorted)
      true
    } else {
      // 4 cases
      // s1 is/isn't in S1
      // s2 is/isn't in S2
    
    // Start with the ones that don't add anything to the sets
//    val cParted = c partition(x => x exists(y => S1.contains(y._1) && S2.contains(y._2)))
    val cParted = c filterNot(x => x exists(y => S1.contains(y._1) && S2.contains(y._2)))
    
//      val cSorted = cParted._2 sortWith (_.length < _.length)
      val cSorted = cParted sortWith (_.length < _.length)
      if (cSorted.isEmpty)
      r(cSorted,S1,S2)
      else
 //     c.head exists (ses => r(c.tail, S1 + ses._1, S2 + ses._2))
  //   cSorted.head exists (ses => r(cSorted.tail, S1 + ses._1, S2 + ses._2))
     (cSorted.head filter (ses => r(cSorted.tail, S1 + ses._1, S2 + ses._2))).size > 0
    }
  }                                               //> r: (c: IndexedSeq[List[(Int, Int)]], S1: Set[Int], S2: Set[Int])Boolean

covers(52).length                                 //> res4: Int = 12

  (1 to 15) map covers                            //> res5: scala.collection.immutable.IndexedSeq[List[(Int, Int)]] = Vector(List
                                                  //| ((1,1), (1,2), (2,1)), List((1,1), (1,2), (2,1), (1,3), (3,1)), List((1,2),
                                                  //|  (2,1), (1,3), (3,1), (1,4), (2,2), (4,1)), List((1,3), (3,1), (1,4), (2,2)
                                                  //| , (4,1), (1,5), (5,1)), List((1,4), (2,2), (4,1), (1,5), (5,1), (1,6), (2,3
                                                  //| ), (3,2), (6,1)), List((1,5), (5,1), (1,6), (2,3), (3,2), (6,1), (1,7), (7,
                                                  //| 1)), List((1,6), (2,3), (3,2), (6,1), (1,7), (7,1), (1,8), (2,4), (4,2), (8
                                                  //| ,1)), List((1,7), (7,1), (1,8), (2,4), (4,2), (8,1), (1,9), (3,3), (9,1)), 
                                                  //| List((1,8), (2,4), (4,2), (8,1), (1,9), (3,3), (9,1), (1,10), (2,5), (5,2),
                                                  //|  (10,1)), List((1,9), (3,3), (9,1), (1,10), (2,5), (5,2), (10,1), (1,11), (
                                                  //| 11,1)), List((1,10), (2,5), (5,2), (10,1), (1,11), (11,1), (1,12), (2,6), (
                                                  //| 3,4), (4,3), (6,2), (12,1)), List((1,11), (11,1), (1,12), (2,6), (3,4), (4,
                                                  //| 3), (6,2), (12,1), (1,13), (13,1)), List((1,12), (2,6), (3,4), (4,3), (6,2)
                                                  //| , (12,1), (1,13), (13,1), (1,14), (2,7), (7,2), (14,1)), List((1,13), (13,1
                                                  //| ), (1,14), (2,7), (7,2), (14,1), (1,15), (3,5), (5,3), (15,1)), List((1,14)
                                                  //| , (2,7), (7,2), (14,1), (1,15), (3,5), (5,3), (15,1), (1,16), (2,8), (4,4),
                                                  //|  (8,2), (16,1)))
  
  val S1: Set[Int] = Set(1, 2,3)                  //> S1  : Set[Int] = Set(1, 2, 3)
  val S2: Set[Int] = Set(2,3,4)                   //> S2  : Set[Int] = Set(2, 3, 4)
    val cParted =  (1 to 15) map covers partition(x => x exists(y => S1.contains(y._1) && S2.contains(y._2)))
                                                  //> cParted  : (scala.collection.immutable.IndexedSeq[List[(Int, Int)]], scala.
                                                  //| collection.immutable.IndexedSeq[List[(Int, Int)]]) = (Vector(List((1,1), (1
                                                  //| ,2), (2,1)), List((1,1), (1,2), (2,1), (1,3), (3,1)), List((1,2), (2,1), (1
                                                  //| ,3), (3,1), (1,4), (2,2), (4,1)), List((1,3), (3,1), (1,4), (2,2), (4,1), (
                                                  //| 1,5), (5,1)), List((1,4), (2,2), (4,1), (1,5), (5,1), (1,6), (2,3), (3,2), 
                                                  //| (6,1)), List((1,5), (5,1), (1,6), (2,3), (3,2), (6,1), (1,7), (7,1)), List(
                                                  //| (1,6), (2,3), (3,2), (6,1), (1,7), (7,1), (1,8), (2,4), (4,2), (8,1)), List
                                                  //| ((1,7), (7,1), (1,8), (2,4), (4,2), (8,1), (1,9), (3,3), (9,1)), List((1,8)
                                                  //| , (2,4), (4,2), (8,1), (1,9), (3,3), (9,1), (1,10), (2,5), (5,2), (10,1)), 
                                                  //| List((1,9), (3,3), (9,1), (1,10), (2,5), (5,2), (10,1), (1,11), (11,1)), Li
                                                  //| st((1,10), (2,5), (5,2), (10,1), (1,11), (11,1), (1,12), (2,6), (3,4), (4,3
                                                  //| ), (6,2), (12,1)), List((1,11), (11,1), (1,12), (2,6), (3,4), (4,3), (6,2),
                                                  //|  (12,1), (1,13), (13,1)), List((1,12), (2,6), (3,4), (4,3), (6,2), (12,1), 
                                                  //| (1,13), (13,1), (1,14), (2,7), (7,2), (14,1))),Vector(List((1,13), (13,1), 
                                                  //| (1,14), (2,7), (7,2), (14,1), (1,15), (3,5), (5,3), (15,1)), List((1,14), (
                                                  //| 2,7), (7,2), (14,1), (1,15), (3,5), (5,3), (15,1), (1,16), (2,8), (4,4), (8
                                                  //| ,2), (16,1))))
   (1 to 60) map covers sortWith (_.length < _.length) map (x => x.head._2 + 1)
                                                  //> res6: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 2, 3, 4, 6, 5,
                                                  //|  8, 10, 7, 12, 14, 18, 22, 38, 58, 9, 16, 26, 11, 13, 20, 28, 30, 32, 34, 4
                                                  //| 2, 46, 52, 15, 17, 24, 50, 19, 21, 23, 27, 33, 40, 44, 54, 25, 36, 37, 48, 
                                                  //| 29, 31, 39, 43, 45, 47, 51, 53, 56, 57, 60, 35, 41, 59, 49, 55)
                                                  
  covers(55)                                      //> res7: List[(Int, Int)] = List((1,54), (2,27), (3,18), (6,9), (9,6), (18,3),
                                                  //|  (27,2), (54,1), (1,55), (5,11), (11,5), (55,1), (1,56), (2,28), (4,14), (7
                                                  //| ,8), (8,7), (14,4), (28,2), (56,1))
   r((1 to 57) map covers, Set(), Set())          //> Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 3, 4, 6, 7, 9)
                                                  //| List(1, 5, 6, 7, 8, 13)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| Solution found!
                                                  //| List(2, 5, 7, 8, 9, 13)
                                                  //| List(1, 3, 4, 6, 7, 9)
                                                  //| res8: Boolean = true
   
   
}