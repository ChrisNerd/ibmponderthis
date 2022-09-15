object ponder {
/*
 Let A be a set of integers of size 2n . It is a known theorem that every such set contains a subset B\subseteq A of size n such that the sum \sum_{b\in B}b is divisible by n .

 For example, for n=7 , given the set A=[70, 18, 15, 74, 71, 40, 92, 54, 23, 82, 24, 6, 73, 62] we can find a subset B=[71, 82, 24, 62, 54, 23, 6] whose elements sum to 322=7 X 46 .

 We add an additional constraint on B . If A=[a_1, a_2,\ldots, a_{2n}] then B must contain *exactly* one element from each pair of elements (a_{2i-1},a_{2i}) for 1 <= i <= n .

 In the above example, B does not satisfy this constraint; both 23 and 82 (elements 9,10, i.e. i=5 ) are in B .
 But we can find another solution satisfying the additional constaint: B = [70, 74, 71, 92, 23, 24, 73]
 Provide a subset of the correct size and sum satisfying the additional constraint. Give your solution in the same list format as the set A.
 */
 // Non-bonus A
 val A = IndexedSeq(194, 236, 291, 430, 1, 721, 486, 819, 2, 44, 196, 238, 293, 529, 584, 45, 197, 433, 101, 46, 586, 337, 5, 725, 102, 822, 781, 144, 6, 920, 394, 242, 976, 533, 492, 244, 493, 632, 784, 729, 106, 536, 591, 149, 10, 537, 204, 634, 689, 150, 399, 538, 690, 926, 884, 54, 206, 539, 400, 831, 207, 735, 401, 832, 498, 929, 789, 251, 111, 542, 208, 639, 305, 736, 112, 930, 500, 349, 791, 931, 888, 156, 113, 253, 501, 933, 695, 62, 503, 159, 891, 838, 116, 935, 989, 742, 505, 937, 699, 841, 893, 551, 894, 842, 22, 67, 119, 358, 313, 843, 604, 359, 895, 553, 217, 747, 702, 941, 993, 457, 121, 70, 218, 555, 800, 168, 25, 362, 26, 653, 414, 363, 125, 753, 29, 949, 514, 950, 805, 78, 999, 757, 418, 467, 31, 80, 128, 468, 613, 857, 807, 276, 323, 761, 227, 858, 712, 277, 325, 374, 616, 665, 810, 763, 132, 86, 36, 474, 424, 862, 522, 961, 135, 89, 426, 186, 39, 672, 233, 866, 330, 770, 815, 287, 912, 384, 137, 191, 525, 676, 41, 192, 623, 581)
                                                  //> A  : IndexedSeq[Int] = Vector(194, 236, 291, 430, 1, 721, 486, 819, 2, 44, 
                                                  //| 196, 238, 293, 529, 584, 45, 197, 433, 101, 46, 586, 337, 5, 725, 102, 822,
                                                  //|  781, 144, 6, 920, 394, 242, 976, 533, 492, 244, 493, 632, 784, 729, 106, 5
                                                  //| 36, 591, 149, 10, 537, 204, 634, 689, 150, 399, 538, 690, 926, 884, 54, 206
                                                  //| , 539, 400, 831, 207, 735, 401, 832, 498, 929, 789, 251, 111, 542, 208, 639
                                                  //| , 305, 736, 112, 930, 500, 349, 791, 931, 888, 156, 113, 253, 501, 933, 695
                                                  //| , 62, 503, 159, 891, 838, 116, 935, 989, 742, 505, 937, 699, 841, 893, 551,
                                                  //|  894, 842, 22, 67, 119, 358, 313, 843, 604, 359, 895, 553, 217, 747, 702, 9
                                                  //| 41, 993, 457, 121, 70, 218, 555, 800, 168, 25, 362, 26, 653, 414, 363, 125,
                                                  //|  753, 29, 949, 514, 950, 805, 78, 999, 757, 418, 467, 31, 80, 128, 468, 613
                                                  //| , 857, 807, 276, 323, 761, 227, 858, 712, 277, 325, 374, 616, 665, 810, 763
                                                  //| , 132, 86, 36, 474, 424, 862, 522, 961, 135, 89, 426, 186, 39, 672, 233, 86
                                                  //| 6, 330, 770, 815, 287, 912, 384, 137, 191, 525, 676, 41, 192, 623, 581)
 // Bonus A
 //val A = IndexedSeq(900, 523, 1, 823, 151, 74, 301, 75, 152, 76, 752, 826, 902, 677, 4, 827, 304, 978, 604, 229, 455, 379, 755, 831, 905, 82, 6, 532, 456, 383, 606, 833, 906, 983, 7, 84, 308, 534, 758, 834, 908, 984, 9, 235, 609, 985, 909, 86, 610, 236, 760, 386, 311, 836, 461, 687, 611, 538, 761, 239, 612, 989, 762, 90, 313, 540, 463, 690, 613, 840, 164, 91, 464, 241, 764, 391, 914, 991, 465, 392, 615, 842, 915, 94, 166, 694, 466, 994, 916, 695, 317, 96, 618, 996, 768, 97, 169, 397, 320, 697, 620, 98, 770, 700, 921, 101, 323, 851, 623, 702, 923, 852, 174, 253, 625, 554, 26, 854, 926, 105, 177, 255, 478, 405, 778, 705, 179, 556, 779, 706, 180, 707, 630, 108, 930, 558, 31, 708, 631, 858, 781, 559, 482, 411, 932, 561, 483, 711, 933, 262, 34, 862, 184, 113, 784, 413, 934, 713, 186, 414, 337, 265, 637, 565, 787, 266, 339, 117, 190, 267, 490, 567, 790, 717, 940, 869, 491, 120, 641, 270, 642, 720, 792, 121, 193, 271, 643, 872, 344, 123, 944, 124, 195, 274, 495, 424, 795, 724, 945, 275, 196, 575, 796, 725, 47, 875, 947, 426, 798, 277, 948, 877, 49, 428, 199, 129, 799, 879, 200, 130, 350, 580, 501, 880, 801, 131, 951, 281, 53, 731, 203, 432, 54, 133, 205, 134, 805, 284, 806, 734, 507, 884, 359, 435, 60, 885, 960, 436, 61, 137, 361, 287, 511, 587, 811, 288, 212, 588, 662, 888, 363, 290, 963, 590, 364, 740, 964, 141, 365, 291, 815, 592, 816, 892, 817, 143, 68, 593, 518, 744, 69, 595, 519, 895, 669, 146, 819, 897, 71, 148, 72, 298, 372, 598, 672, 149, 73, 299, 373, 599)
 // A given in the question as an example.
//  val A = IndexedSeq(70, 18, 15, 74, 71, 40, 92, 54, 23, 82, 24, 6, 73, 62)
 A.size                                           //> res0: Int = 194
 A.sum                                            //> res1: Int = 92025
 val twon = A.size                                //> twon  : Int = 194
 val n = twon / 2                                 //> n  : Int = 97
 
 def allBinarySequencesFromBigInt(n: Int) : Iterator[IndexedSeq[Boolean]] = Iterator.iterate(BigInt(0))(_ + 1).takeWhile( _.toString(2).size <= n)
  .map( b => b.toString(2).reverse.padTo(n, '0').reverse // convert the BigInt to a binary String and 0-pad it to length n.
  .map( s => if (s == '0') false else true)) // convert the binary string to a Sequence of trues and falses
                                                  //> allBinarySequencesFromBigInt: (n: Int)Iterator[IndexedSeq[Boolean]]
  
 
 allBinarySequencesFromBigInt(4).foreach(println) //> Vector(false, false, false, false)
                                                  //| Vector(false, false, false, true)
                                                  //| Vector(false, false, true, false)
                                                  //| Vector(false, false, true, true)
                                                  //| Vector(false, true, false, false)
                                                  //| Vector(false, true, false, true)
                                                  //| Vector(false, true, true, false)
                                                  //| Vector(false, true, true, true)
                                                  //| Vector(true, false, false, false)
                                                  //| Vector(true, false, false, true)
                                                  //| Vector(true, false, true, false)
                                                  //| Vector(true, false, true, true)
                                                  //| Vector(true, true, false, false)
                                                  //| Vector(true, true, false, true)
                                                  //| Vector(true, true, true, false)
                                                  //| Vector(true, true, true, true)
 
 // Same thing as above but without BigInts and done recursively.
 def allBinarySequenciesofLengthN(n: Int) : Iterator[IndexedSeq[Boolean]] = n match {
  case 0 => Iterator(IndexedSeq())
  case _ => for{
   h <- Iterator(false,true)
   rest <- allBinarySequenciesofLengthN(n-1)
  } yield h +: rest
 }                                                //> allBinarySequenciesofLengthN: (n: Int)Iterator[IndexedSeq[Boolean]]
 
 allBinarySequenciesofLengthN(4).foreach(println) //> Vector(false, false, false, false)
                                                  //| Vector(false, false, false, true)
                                                  //| Vector(false, false, true, false)
                                                  //| Vector(false, false, true, true)
                                                  //| Vector(false, true, false, false)
                                                  //| Vector(false, true, false, true)
                                                  //| Vector(false, true, true, false)
                                                  //| Vector(false, true, true, true)
                                                  //| Vector(true, false, false, false)
                                                  //| Vector(true, false, false, true)
                                                  //| Vector(true, false, true, false)
                                                  //| Vector(true, false, true, true)
                                                  //| Vector(true, true, false, false)
                                                  //| Vector(true, true, false, true)
                                                  //| Vector(true, true, true, false)
                                                  //| Vector(true, true, true, true)
 
 // Select the elements from A to create B. The elements are selected based on the n-length Boolean array.
 val Bcandidates = allBinarySequencesFromBigInt(n)/*allBinarySequenciesofLengthN(n)*/.map{ bSequence =>
  (bSequence.zipWithIndex.map{ case(bool, index) => if (bool) A(2*index+1) else A(2*index) }, bSequence)
 }                                                //> Bcandidates  : Iterator[(IndexedSeq[Int], IndexedSeq[Boolean])] = non-empty
                                                  //|  iterator
 // A could have been reduced mod n at the very beginning.
 A.map ( _ % n )                                  //> res2: IndexedSeq[Int] = Vector(0, 42, 0, 42, 1, 42, 1, 43, 2, 44, 2, 44, 2,
                                                  //|  44, 2, 45, 3, 45, 4, 46, 4, 46, 5, 46, 5, 46, 5, 47, 6, 47, 6, 48, 6, 48, 
                                                  //| 7, 50, 8, 50, 8, 50, 9, 51, 9, 52, 10, 52, 10, 52, 10, 53, 11, 53, 11, 53, 
                                                  //| 11, 54, 12, 54, 12, 55, 13, 56, 13, 56, 13, 56, 13, 57, 14, 57, 14, 57, 14,
                                                  //|  57, 15, 57, 15, 58, 15, 58, 15, 59, 16, 59, 16, 60, 16, 62, 18, 62, 18, 62
                                                  //| , 19, 62, 19, 63, 20, 64, 20, 65, 20, 66, 21, 66, 22, 67, 22, 67, 22, 67, 2
                                                  //| 2, 68, 22, 68, 23, 68, 23, 68, 23, 69, 24, 70, 24, 70, 24, 71, 25, 71, 26, 
                                                  //| 71, 26, 72, 28, 74, 29, 76, 29, 77, 29, 78, 29, 78, 30, 79, 31, 80, 31, 80,
                                                  //|  31, 81, 31, 82, 32, 82, 33, 82, 33, 83, 34, 83, 34, 83, 34, 84, 35, 86, 36
                                                  //| , 86, 36, 86, 37, 88, 38, 89, 38, 89, 39, 90, 39, 90, 39, 91, 39, 93, 39, 9
                                                  //| 3, 40, 94, 40, 94, 41, 95, 41, 96)
 // Interesing sorting order, maybe a clue?
 A.map ( _ % n ).grouped(2).foreach(println)      //> Vector(0, 42)
                                                  //| Vector(0, 42)
                                                  //| Vector(1, 42)
                                                  //| Vector(1, 43)
                                                  //| Vector(2, 44)
                                                  //| Vector(2, 44)
                                                  //| Vector(2, 44)
 
 // Shift it by k to the right. Implement at circular array. Rotate.
 // k is guarenteed to be 0<=k<n by the caller so it's not necessary to do the mod here as well.
 def rotateBy(A: IndexedSeq[BigInt], k: Int): IndexedSeq[BigInt] = A.drop(A.size - k%A.size) ++ A.take(A.size - k%A.size)
 
 rotateBy(IndexedSeq(0,1,2,3,4),2)
 
 def rotateArrayAndPrependStreamsWithInt(A: IndexedSeq[Stream[List[Int]]], k: Int): IndexedSeq[Stream[List[Int]]] = {
  val (first, second) = A.map(_.map(k +: _)).splitAt(A.size - k%A.size)
  second ++ first
 }
 
 // Count the number of ways to sum up to each of the n sums, given the first k pairs of A. Adding another pair from A
 // increases the counts. The returned value, sumsSoFar, is an array of size n, where each elements represents the number of ways to sum to that index.
 // The "zero" in the foldleft is a 1 followed by n-1 0s because there is precisely 1 way to sum to 0 from the empty list.
 // 1 0 0 0 .... for 150 terms indicates that there's 1 way to sum to 0, and no other possibilities.
 // 0 1 0 2 .... indicates there is 1 way to sum to 1; and 2 ways to sum to 3.
 // if we have the option to add 3 (p1), say, we'd just shift that vector to the right by 3.
 // 0 0 0 0 1 0 2 ... this means that the ways to sum to 1 are now ways to sum to 4; and the 2 ways to sum to 3 are now 2 ways to sum to 6.
 // we repeat to create another vector with the other option to add (say 5)(p2)
 // 0 0 0 0 0 0 1 0 2 ...
 // And we finish by combining them, adding them element-wise. This is because all the combinations are disjoint (assuming unique elements of A, or at least
 // the two elements making that pair are distinguishable.
 val numberOfWaysToSumToIndex = A.map( _ % n).grouped(2).foldLeft(BigInt(1) +: IndexedSeq.fill(n-1)(BigInt(0))){ case (sumsSoFar, IndexedSeq(p1,p2)) =>
   (rotateBy(sumsSoFar, p1), rotateBy(sumsSoFar, p2)).zipped.map( _ + _ )
 }
 numberOfWaysToSumToIndex.head
 /*val generateSolutionsViaListingWaysToSumToIndex = A.grouped(2).foldLeft( Stream(List[Int]()) +: IndexedSeq.fill(n-1)(Stream()) ) { case (sumsSoFar, IndexedSeq(p1,p2)) =>
  (rotateArrayAndPrependStreamsWithInt(sumsSoFar, p1), rotateArrayAndPrependStreamsWithInt(sumsSoFar, p2)).zipped.map( _ ++ _ )
 } */
 val generateSolutionsViaListingWaysToSumToIndex = A.grouped(2).foldLeft( Stream(List[Int]()) +: IndexedSeq.fill(n-1)(Stream()) ) { case (sumsSoFar, ps) =>
  ps.map(rotateArrayAndPrependStreamsWithInt(sumsSoFar, _)).transpose.map( _.reduce(_ ++ _) )
 }
 val solutionsOfSumsEqual0ModN = generateSolutionsViaListingWaysToSumToIndex.head
 // solutionsOfSumsEqual0ModN.size
 // Need to get reversed because we built it by prepending elements to the list.
 solutionsOfSumsEqual0ModN.take(10000).foreach(x=>println(x.reverse))
 
 
 val r = BigDecimal(numberOfWaysToSumToIndex.head) / BigDecimal(2).pow(n)
 val rInverse = BigDecimal(2).pow(n) / BigDecimal(numberOfWaysToSumToIndex.head)
 numberOfWaysToSumToIndex.sum
 BigDecimal(2).pow(n)

 //A.map ( _ % n ).grouped(2).foreach{x => println(x(0) + " " + (150-x(1)))}
 //A.map ( _ % n ).grouped(2).foreach{x => println(x(0) + " " + (150-x(1)) + " sum " + (x(0) + 150 - x(1)))}
 
 val solutions = Bcandidates.zipWithIndex.filter{ case ((bCandidates, booleans), i) =>
  //if (i %100000 == 0 ) println(Bcandidates + " sum " + bCandidates.sum + " mod " + n  + " = " + bCandidates.sum % n)
  bCandidates.sum % n == 0
 }
 /*
 // Would never complete other than the simple n=7 case, in which case it varifies the folding solution of 35 solutions.
 val solList = solutions.toList
 solList.size
 */
 solutions.foreach(solution=>println(solution._1 + " at position " + solution._2))
 
 /*
 n=7 case
 (Vector(70, 15, 71, 92, 23, 24, 62),Vector(false, false, false, false, false, false, true)) at position 1
 Number of solutions: 35
 ratio:
 0.2734375
 
 Non-bonus solution
 [194, 291, 1, 486, 2, 196, 293, 584, 197, 101, 586, 5, 102, 781, 6, 394, 976, 492, 493, 784, 106, 591, 10, 204, 689, 399, 690, 884, 206, 400, 2
                                                   07, 401, 498, 789, 111, 208, 305, 112, 500, 791, 888, 113, 501, 695, 503, 8
                                                   91, 116, 989, 505, 699, 893, 894, 22, 119, 313, 604, 895, 217, 702, 993, 12
                                                   1, 218, 800, 25, 26, 414, 125, 29, 514, 805, 999, 418, 31, 128, 613, 807, 3
                                                   23, 227, 712, 325, 616, 810, 132, 36, 424, 961, 89, 186, 672, 866, 330, 287
                                                   , 384, 191, 676, 192, 581),Vector(false, false, false, false, false, false,
                                                    false, false, false, false, false, false, false, false, false, false, fals
                                                   e, false, false, false, false, false, false, false, false, false, false, fa
                                                   lse, false, false, false, false, false, false, false, false, false, false,
                                                   false, false, false, false, false, false, false, false, false, false, false
                                                   , false, false, false, false, false, false, false, false, false, false, fal
                                                   se, false, false, false, false, false, false, false, false, false, false, f
                                                   alse, false, false, false, false, false, false, false, false, false, false,
                                                    false, false, false, false, true, true, true, true, true, false, true, tru
                                                   e, true, true, true, true)) at position 4031
 Number of solutions
 1640967457544366442515945468
 ratio:
 0.01035596059197336935926826952663592
 
 Bonus solution is [900, 1, 151, 301, 152, 752, 902, 4, 304, 604, 455, 755, 905, 6, 456, 606, 906, 7, 308, 758, 908, 9, 609, 909, 610, 760, 311, 461, 611, 761, 612, 762, 313, 463, 613, 164, 464, 764, 914, 465, 615, 915, 166, 466, 916, 317, 618, 768, 169, 320, 620, 770, 921, 323, 623, 923, 174, 625, 26, 926, 177, 478, 778, 179, 779, 180, 630, 930, 31, 631, 781, 482, 932, 483, 933, 34, 184, 784, 934, 186, 337, 637, 787, 339, 190, 490, 790, 940, 491, 641, 642, 792, 193, 643, 344, 944, 195, 495, 795, 945, 196, 796, 47, 947, 798, 948, 49, 199, 799, 200, 350, 501, 801, 951, 53, 203, 54, 205, 805, 806, 507, 359, 60, 960, 61, 361, 511, 811, 212, 662, 290, 590, 740, 141, 291, 592, 892, 143, 68, 744, 595, 895, 146, 897, 148, 298, 598, 149, 299, 599]
),104652)
104652 in binary is 0b11111111011111111111
16035579592615422135362088368615933413381520 solutions
representing
0.01123531652884518338741608441224707967254089 as a fraction of all
 */
}