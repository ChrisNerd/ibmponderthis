object ponder {
  def euclidsFormulaForPythagoreanTriplets(m: Int, n: Int) /*: (Int, Int, Int)*/ ={
   assert(m>n)
   assert(m>0)
   assert(n>0)
   
  ( m*m - n*n, 2*m*n, m*m +n*n, m,n)
 }                                                //> euclidsFormulaForPythagoreanTriplets: (m: Int, n: Int)(Int, Int, Int, Int, I
                                                  //| nt)
 euclidsFormulaForPythagoreanTriplets(4,3)        //> res0: (Int, Int, Int, Int, Int) = (7,24,25,4,3)
  
 def gcd(a: Int,b: Int): Int = {
       if(b ==0) a else gcd(b, a%b)
 }                                                //> gcd: (a: Int, b: Int)Int

 //4*a = n^2
 // a+b+c+d > a+a+a+a = 4a =n^2
// val maxN = math.sqrt(4*12000)
 val maxA = 120000                                //> maxA  : Int = 120000
 val maxN = math.sqrt(4*maxA)                     //> maxN  : Double = 692.8203230275509

 val primitiveTriples = for{
   n <- Stream.from(1).takeWhile(_ < maxN)
   m <- Stream.from(n+1).takeWhile(euclidsFormulaForPythagoreanTriplets(_, n)._3 < maxN)
   //  is primitive if and only if m and n are coprime and not both odd.
   if (gcd(m,n) == 1 && !( m % 2 != 0 && n % 2 != 0))
  } yield euclidsFormulaForPythagoreanTriplets(m, n)
                                                  //> primitiveTriples  : scala.collection.immutable.Stream[(Int, Int, Int, Int, I
                                                  //| nt)] = Stream((3,4,5,2,1), ?)
 primitiveTriples.take(100).foreach(println)      //> (3,4,5,2,1)
                                                  //| (15,8,17,4,1)
                                                  //| (35,12,37,6,1)
                                                  //| (63,16,65,8,1)
                                                  //| (99,20,101,10,1)
                                                  //| (143,24,145,12,1)
                                                  //| (195,28,197,14,1)
                                                  //| (255,32,257,16,1)
                                                  //| (323,36,325,18,1)
                                                  //| (399,40,401,20,1)
                                                  //| (483,44,485,22,1)
                                                  //| (575,48,577,24,1)
                                                  //| (675,52,677,26,1)
                                                  //| (5,12,13,3,2)
                                                  //| (21,20,29,5,2)
                                                  //| (45,28,53,7,2)
                                                  //| (77,36,85,9,2)
                                                  //| (117,44,125,11,2)
                                                  //| (165,52,173,13,2)
                                                  //| (221,60,229,15,2)
                                                  //| (285,68,293,17,2)
                                                  //| (357,76,365,19,2)
                                                  //| (437,84,445,21,2)
                                                  //| (525,92,533,23,2)
                                                  //| (621,100,629,25,2)
                                                  //| (7,24,25,4,3)
                                                  //| (55,48,73,8,3)
                                                  //| (91,60,109,10,3)
                                                  //| (187,84,205,14,3)
                                                  //| (247,96,265,16,3)
                                                  //| (391,120,409,20,3)
                                                  //| (475,132,493,22,3)
                                                  //| (667,156,685,26,3)
                                                  //| (9,40,41,5,4)
                                                  //| (33,56,65,7,4)
                                                  //| (65,72,97,9,4)
                                                  //| (105,88,137,11,4)
                                                  //| (153,104,185,13,4)
                                                  //| (209,120,241,15,4)
                                                  //| (273,136,305,17,4)
                                                  //| (345,152,377,19,4)
                                                  //| (425,168,457,21,4)
                                                  //| (513,184,545,23,4)
                                                  //| (609,200,641,25,4)
                                                  //| (11,60,61,6,5)
                                                  //| (39,80,89,8,5)
                                                  //| (119,120,169,12,5)
                                                  //| (171
                                                  //| Output exceeds cutoff limit.
 
 
 val allTriplets =
   primitiveTriples.takeWhile(_._3 < maxN).flatMap{ primitiveTriplet => {
    Stream.from(1).map{ k => (k*primitiveTriplet._1, k*primitiveTriplet._2, k*primitiveTriplet._3)}.takeWhile(_._3 < maxN)
   }
  }                                               //> allTriplets  : scala.collection.immutable.Stream[(Int, Int, Int)] = Stream(
                                                  //| (3,4,5), ?)
 primitiveTriples.takeWhile(_._3 < maxN).size     //> res1: Int = 110
 allTriplets.take(10).foreach(println)            //> (3,4,5)
                                                  //| (6,8,10)
                                                  //| (9,12,15)
                                                  //| (12,16,20)
                                                  //| (15,20,25)
                                                  //| (18,24,30)
                                                  //| (21,28,35)
                                                  //| (24,32,40)
                                                  //| (27,36,45)
                                                  //| (30,40,50)
 val grouped = allTriplets.groupBy(_._3).filter{ _._2.size >= 3}
                                                  //> grouped  : scala.collection.immutable.Map[Int,scala.collection.immutable.St
                                                  //| ream[(Int, Int, Int)]] = Map(629 -> Stream((555,296,629), (595,204,629), (6
                                                  //| 21,100,629), (429,460,629)), 365 -> Stream((219,292,365), (357,76,365), (27
                                                  //| 5,240,365), (27,364,365)), 555 -> Stream((333,444,555), (525,180,555), (459
                                                  //| ,312,555), (171,528,555)), 481 -> Stream((455,156,481), (185,444,481), (319
                                                  //| ,360,481), (31,480,481)), 170 -> Stream((102,136,170), (150,80,170), (154,7
                                                  //| 2,170), (26,168,170)), 533 -> Stream((205,492,533), (525,92,533), (117,520,
                                                  //| 533), (435,308,533)), 500 -> Stream((300,400,500), (468,176,500), (140,480,
                                                  //| 500)), 340 -> Stream((204,272,340), (300,160,340), (308,144,340), (52,336,3
                                                  //| 40)), 185 -> Stream((111,148,185), (175,60,185), (153,104,185), (57,176,185
                                                  //| )), 565 -> Stream((339,452,565), (493,276,565), (75,560,565), (403,396,565)
                                                  //| ), 125 -> Stream((75,100,125), (117,44,125), (35,120,125)), 325 -> Stream((
                                                  //| 195,260,325), (315,80,3
                                                  //| Output exceeds cutoff limit.
                                                  
 // now we have candidate ns, which are n^2=a+b+c+d
 // where a+b = another n ^2
 // For example 130^2 = 78^2+104^2
 // find a+b = 78^2 *
 // and c+d=104^2 *
 // and also 130^2= 126^2+32^2
 // so a+c = 126^2 *
 // and b+d = 32^2 *
 // So it's 4 equations (the *s) 4 unknowns (abcd)
 // [1100] *[abcd]^t = [78 104 126 32]^2t
 // [0011]
 // [1010]
 // [0101]
 // Hmm, singular matrix...
 // OK, take 130 -> Stream((78,104,130), (126,32,130), (50,120,130), (66,112,130))
 // So we have to take .subset(3) of it, to give us 6 numbers to work with.
 // For example take 78,104, 50,120, 66, 112.
 // Make
 // a+b =78
 // a+c = 104
 // a+d = 50
 // b+c = 120
 // b+d = 66
 // c+d = 112
 
 // 6 equations, 4 unknowns.
 // 1100
 // 1010
 // 1001
 // 0110
 // 0101
 // 0011
 // b->b
 // a->c
 // c->d
 // d->a
 // WHOA! The order matters (of the 78,104,...) some orders have solutions, some don't!!!
 // OH obviously...
 // We have the pythagorean triplet (78,104,130), so that means that if a+b=78^2, then it must be the other two, c+d that give 104^2
 // So our matrix is alternating rows
 // 1100 t0
 // 0011 t1
 // 1010 t2 (maybe swapped with t3)
 // 0101 t3
 // 1001 t4
 // 0110 t5
 
 // Maybe we have to enforce a<b<c<d
 // Therefore a+b < a+c < a+d < b+d < c+d, but where is b+c??  Well, we have a+c < b+c < b+d, but we don't know how it comepares to a+d. Only two choices.
 // Of the two, one will have some negative numbers.
 // Well, we can putze around with Penrose Inverses, and Gaussian Elimination, or we can trial solve on O(n).
 // Guess a. (only need to go up to smallest(t)/2
 // Can easily solve for b, c and d! Now verify the other 3 equations.

def solveABCD(t2: List[Int])/*: IndexedSeq[(Int, Int, Int, Int)]*/ =
{
 val t = t2.map(x=>x*x)
 (10 to t.max).map(a => {
  val b= t(0)-a
  val c= t(2)-a
  val d= t(4)-a
  (a, b, c, d)
  }).filter{ case(a,b,c,d) =>
  c + d == t(1) &&  b + d ==t(3) && b+c==t(5) && b>0 && c>0 && d>0 && b< maxA && c< maxA && d < maxA
 }
}                                                 //> solveABCD: (t2: List[Int])scala.collection.immutable.IndexedSeq[(Int, Int, 
                                                  //| Int, Int)]
  
 solveABCD(List(50,66,78,104,112,120))            //> res2: scala.collection.immutable.IndexedSeq[(Int, Int, Int, Int)] = Vector(
                                                  //| )
 solveABCD(List(50,66,78,104,120,112))            //> res3: scala.collection.immutable.IndexedSeq[(Int, Int, Int, Int)] = Vector(
                                                  //| )
 solveABCD(List(50,120,78,104,66,112))            //> res4: scala.collection.immutable.IndexedSeq[(Int, Int, Int, Int)] = Vector(
                                                  //| )
 solveABCD(List(50,120,78,104,112,66))            //> res5: scala.collection.immutable.IndexedSeq[(Int, Int, Int, Int)] = Vector(
                                                  //| (2114,386,3970,10430))

// Oh wait, there's 4 ways to arrange the equations. ab=t0 and cd=t1, that's fixed. But then we can flip either the middle pair, or the last pair, or both.
// There are 3 pairs.
// (t0, t1)  --> don't flip, no loss of generality
// (t2, t3)  --> can flip (t3,t2)
// (t4, t5)  --> can flip (t5,t4)
val s1 = grouped.flatMap{ case(k,v) => v.toSet.subsets(3).flatMap{ p =>
solveABCD(List(p.toList(0)._1,p.toList(0)._2, p.toList(1)._1, p.toList(1)._2,p.toList(2)._1, p.toList(2)._2))
}}                                                //> s1  : scala.collection.immutable.Iterable[(Int, Int, Int, Int)] = List((237
                                                  //| 7,9944,21032,872), (617,63392,26608,15008), (11817,52192,15408,26208), (234
                                                  //| 5,61664,5936,35680), (6017,57992,9608,32008), (12881,2248,22088,4808), (604
                                                  //| 17,27792,87808,4608), (56617,31592,8408,84008), (8073,12952,110952,10152), 
                                                  //| (9508,39776,84128,3488), (51524,8992,88352,19232), (28921,4568,45608,13928)
                                                  //| )
val s2 = grouped.flatMap{ case(k,v) => v.toSet.subsets(3).flatMap{ p =>
solveABCD(List(p.toList(0)._1,p.toList(0)._2, p.toList(1)._1, p.toList(1)._2,p.toList(2)._2, p.toList(2)._1))
}}                                                //> s2  : scala.collection.immutable.Iterable[(Int, Int, Int, Int)] = List((459
                                                  //| 4,5810,17906,590), (18376,23240,71624,2360), (8456,15880,1544,41720), (2114
                                                  //| ,3970,386,10430), (39074,10210,83426,4190), (19026,35730,3474,93870))
val s3 = grouped.flatMap{ case(k,v) => v.toSet.subsets(3).flatMap{ p =>
solveABCD(List(p.toList(0)._1,p.toList(0)._2, p.toList(1)._2, p.toList(1)._1,p.toList(2)._1, p.toList(2)._2))
}}                                                //> s3  : scala.collection.immutable.Iterable[(Int, Int, Int, Int)] = List((581
                                                  //| 0,4594,590,17906), (23240,18376,2360,71624), (15880,8456,41720,1544), (3970
                                                  //| ,2114,10430,386), (10210,39074,4190,83426), (35730,19026,93870,3474))
val s4 = grouped.flatMap{ case(k,v) => v.toSet.subsets(3).flatMap{ p =>
solveABCD(List(p.toList(0)._1,p.toList(0)._2, p.toList(1)._2, p.toList(1)._1,p.toList(2)._2, p.toList(2)._1))
}}                                                //> s4  : scala.collection.immutable.Iterable[(Int, Int, Int, Int)] = List((994
                                                  //| 4,2377,872,21032), (63392,617,15008,26608), (52192,11817,26208,15408), (616
                                                  //| 64,2345,35680,5936), (57992,6017,32008,9608), (253568,2468,106432,60032), (
                                                  //| 2248,12881,4808,22088), (27792,60417,4608,87808), (31592,56617,84008,8408),
                                                  //|  (154880,36089,5120,1936), (12952,8073,10152,110952), (39776,9508,3488,8412
                                                  //| 8), (8992,51524,19232,88352), (4568,28921,13928,45608))

s1 ++ s2 ++ s3 ++ s4                              //> res6: scala.collection.immutable.Iterable[(Int, Int, Int, Int)] = List((237
                                                  //| 7,9944,21032,872), (617,63392,26608,15008), (11817,52192,15408,26208), (234
                                                  //| 5,61664,5936,35680), (6017,57992,9608,32008), (12881,2248,22088,4808), (604
                                                  //| 17,27792,87808,4608), (56617,31592,8408,84008), (8073,12952,110952,10152), 
                                                  //| (9508,39776,84128,3488), (51524,8992,88352,19232), (28921,4568,45608,13928)
                                                  //| , (4594,5810,17906,590), (18376,23240,71624,2360), (8456,15880,1544,41720),
                                                  //|  (2114,3970,386,10430), (39074,10210,83426,4190), (19026,35730,3474,93870),
                                                  //|  (5810,4594,590,17906), (23240,18376,2360,71624), (15880,8456,41720,1544), 
                                                  //| (3970,2114,10430,386), (10210,39074,4190,83426), (35730,19026,93870,3474), 
                                                  //| (9944,2377,872,21032), (63392,617,15008,26608), (52192,11817,26208,15408), 
                                                  //| (61664,2345,35680,5936), (57992,6017,32008,9608), (253568,2468,106432,60032
                                                  //| ), (2248,12881,4808,22088), (27792,60417,4608,87808), (31592,56617,84008,84
                                                  //| 08), (154880,36089,5120
                                                  //| Output exceeds cutoff limit.
                                                  
val setOfSquaresUpto4X = Stream.from(1).map(x=>x*x).takeWhile(_ < 4*12000).toSet
                                                  //> setOfSquaresUpto4X  : scala.collection.immutable.Set[Int] = Set(10609, 1024
                                                  //| , 1369, 7569, 5329, 21025, 30276, 3969, 29929, 44100, 7396, 9604, 5625, 174
                                                  //| 24, 25921, 11881, 29584, 19044, 11664, 32041, 6241, 3481, 36864, 21316, 980
                                                  //| 1, 11449, 39601, 3249, 841, 5476, 18496, 31684, 25, 16641, 18769, 2916, 275
                                                  //| 56, 11025, 9216, 4761, 37636, 196, 13456, 289, 44521, 10816, 121, 38416, 17
                                                  //| 161, 10201, 45369, 5929, 10404, 41209, 47524, 37249, 2809, 1, 1600, 1521, 2
                                                  //| 0736, 529, 39204, 43264, 12100, 44944, 2601, 24964, 729, 47089, 484, 15129,
                                                  //|  361, 625, 12321, 13924, 21609, 1296, 3600, 34969, 324, 1936, 1089, 12769, 
                                                  //| 9, 169, 256, 25281, 225, 31329, 8100, 6724, 18225, 2304, 22201, 5776, 3364,
                                                  //|  23716, 41616, 28900, 6400, 45796, 38809, 1764, 24336, 6561, 7225, 6084, 64
                                                  //| , 4489, 32400, 40401, 24649, 9025, 33856, 19881, 15876, 4096, 144, 42436, 4
                                                  //| 9, 14161, 43681, 42849, 19321, 16900, 30976, 900, 784, 81, 10000, 2116, 268
                                                  //| 96, 38025, 2025, 22801,
                                                  //| Output exceeds cutoff limit.
val setOfSquaresUpto2X = Stream.from(1).map(x=>x*x).takeWhile(_ < 2*12000).toSet
                                                  //> setOfSquaresUpto2X  : scala.collection.immutable.Set[Int] = Set(10609, 1024
                                                  //| , 1369, 7569, 5329, 21025, 3969, 7396, 9604, 5625, 17424, 11881, 19044, 116
                                                  //| 64, 6241, 3481, 21316, 9801, 11449, 3249, 841, 5476, 18496, 25, 16641, 1876
                                                  //| 9, 2916, 11025, 9216, 4761, 196, 13456, 289, 10816, 121, 17161, 10201, 5929
                                                  //| , 10404, 2809, 1, 1600, 1521, 20736, 529, 12100, 2601, 729, 484, 15129, 361
                                                  //| , 625, 12321, 13924, 21609, 1296, 3600, 324, 1936, 1089, 12769, 9, 169, 256
                                                  //| , 225, 8100, 6724, 18225, 2304, 22201, 5776, 3364, 23716, 6400, 1764, 6561,
                                                  //|  7225, 6084, 64, 4489, 9025, 19881, 15876, 4096, 144, 49, 14161, 19321, 169
                                                  //| 00, 900, 784, 81, 10000, 2116, 2025, 22801, 20164, 8281, 2401, 16129, 8836,
                                                  //|  1156, 1225, 22500, 23104, 17689, 23409, 441, 16, 16384, 576, 8464, 2500, 1
                                                  //| 849, 7921, 4624, 14400, 4225, 14641, 15625, 2209, 5184, 12544, 961, 3136, 4
                                                  //| 900, 3844, 8649, 19600, 1444, 4356, 36, 7744, 17956, 13225, 5041, 4, 2704, 
                                                  //| 400, 6889, 9409, 11236,
                                                  //| Output exceeds cutoff limit.

setOfSquaresUpto4X.size                           //> res7: Int = 219
setOfSquaresUpto2X.size                           //> res8: Int = 154
for {
 a <- 1 to 12000-3
 List(b, c) <- setOfSquaresUpto2X.map(_ - a).takeWhile(_ < 12000).filter( a < _ ).subsets(2).map(_.toList.sorted)
 if (setOfSquaresUpto2X.contains(b + c))
 d <- (setOfSquaresUpto2X.map(_ - a) -- Set(b,c)).takeWhile(_ < 12000).filter( List(a,b,c).max < _)
 if (setOfSquaresUpto4X.contains( a + b + c + d ) && setOfSquaresUpto2X.contains(b + d) && setOfSquaresUpto2X.contains(c + d))
 } yield (a, b, c, d)                             //> res9: scala.collection.immutable.IndexedSeq[(Int, Int, Int, Int)] = Vector(
                                                  //| )
 
 def isSquare(n: Int) = math.sqrt(n) % 1 == 0     //> isSquare: (n: Int)Boolean

 isSquare(25)                                     //> res10: Boolean = true
 isSquare(26)                                     //> res11: Boolean = false
 isSquare(24)                                     //> res12: Boolean = false
 isSquare(361)                                    //> res13: Boolean = true\

 // 12000 choose 4 = 8.63568065997 × 10^14
 (1 to 12000).toSet.subsets(4).filter( setOf4 => isSquare(setOf4.sum) && setOf4.subsets(2).forall(setOf2 => isSquare(setOf2.sum)))
 
                                                   
}