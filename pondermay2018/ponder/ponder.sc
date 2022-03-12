object ponder {
// def main(args: Array[String]) = {

  val coins = ('A' to 'K').toSet                  //> coins  : scala.collection.immutable.Set[Char] = Set(E, J, F, A, I, G, B, C, H
                                                  //| , K, D)
  println("coins.size", coins.size)               //> (coins.size,11)

  // 2 cointerfeit coins
  val IndexedSeqOfAllCounterfeitsCombos = coins.subsets(2).toIndexedSeq
                                                  //> IndexedSeqOfAllCounterfeitsCombos  : scala.collection.immutable.IndexedSeq[s
                                                  //| cala.collection.immutable.Set[Char]] = Vector(Set(E, J), Set(E, F), Set(E, A
                                                  //| ), Set(E, I), Set(E, G), Set(E, B), Set(E, C), Set(E, H), Set(E, K), Set(E, 
                                                  //| D), Set(J, F), Set(J, A), Set(J, I), Set(J, G), Set(J, B), Set(J, C), Set(J,
                                                  //|  H), Set(J, K), Set(J, D), Set(F, A), Set(F, I), Set(F, G), Set(F, B), Set(F
                                                  //| , C), Set(F, H), Set(F, K), Set(F, D), Set(A, I), Set(A, G), Set(A, B), Set(
                                                  //| A, C), Set(A, H), Set(A, K), Set(A, D), Set(I, G), Set(I, B), Set(I, C), Set
                                                  //| (I, H), Set(I, K), Set(I, D), Set(G, B), Set(G, C), Set(G, H), Set(G, K), Se
                                                  //| t(G, D), Set(B, C), Set(B, H), Set(B, K), Set(B, D), Set(C, H), Set(C, K), S
                                                  //| et(C, D), Set(H, K), Set(H, D), Set(K, D))
  println("Combos size ", IndexedSeqOfAllCounterfeitsCombos.size)
                                                  //> (Combos size ,55)

  val experiments =
    for {
      s <- (1 to coins.size / 2)
      left <- coins.subsets(s)
      right <- coins.diff(left).subsets(s)
      if (left.min < right.min)
    } yield (left, right)                         //> experiments  : scala.collection.immutable.IndexedSeq[(scala.collection.immut
                                                  //| able.Set[Char], scala.collection.immutable.Set[Char])] = Vector((Set(E),Set(
                                                  //| J)), (Set(E),Set(F)), (Set(E),Set(I)), (Set(E),Set(G)), (Set(E),Set(H)), (Se
                                                  //| t(E),Set(K)), (Set(J),Set(K)), (Set(F),Set(J)), (Set(F),Set(I)), (Set(F),Set
                                                  //| (G)), (Set(F),Set(H)), (Set(F),Set(K)), (Set(A),Set(E)), (Set(A),Set(J)), (S
                                                  //| et(A),Set(F)), (Set(A),Set(I)), (Set(A),Set(G)), (Set(A),Set(B)), (Set(A),Se
                                                  //| t(C)), (Set(A),Set(H)), (Set(A),Set(K)), (Set(A),Set(D)), (Set(I),Set(J)), (
                                                  //| Set(I),Set(K)), (Set(G),Set(J)), (Set(G),Set(I)), (Set(G),Set(H)), (Set(G),S
                                                  //| et(K)), (Set(B),Set(E)), (Set(B),Set(J)), (Set(B),Set(F)), (Set(B),Set(I)), 
                                                  //| (Set(B),Set(G)), (Set(B),Set(C)), (Set(B),Set(H)), (Set(B),Set(K)), (Set(B),
                                                  //| Set(D)), (Set(C),Set(E)), (Set(C),Set(J)), (Set(C),Set(F)), (Set(C),Set(I)),
                                                  //|  (Set(C),Set(G)), (Set(C),Set(H)), (Set(C),Set(K)), (Set(C),Set(D)), (Set(H)
                                                  //| ,Set(J)), (Set(H),Set(I)
                                                  //| Output exceeds cutoff limit.

  println("experiments.size = ", experiments.size)//> (experiments.size = ,12826)

  val weightCache = collection.mutable.Map[Set[Char], IndexedSeq[Int]]()
                                                  //> weightCache  : scala.collection.mutable.Map[Set[Char],IndexedSeq[Int]] = Map
                                                  //| ()

  def weightOfSide(halfExperiment: Set[Char]): IndexedSeq[Int] = {
    weightCache.getOrElseUpdate(halfExperiment,
      IndexedSeqOfAllCounterfeitsCombos map { counterfeitsPair =>
        halfExperiment.toIndexedSeq.map { weight => if (counterfeitsPair.contains(weight)) 0 else 1 }.sum
      })
  }                                               //> weightOfSide: (halfExperiment: Set[Char])IndexedSeq[Int]

  def signature(experiment: (Set[Char], Set[Char])): IndexedSeq[Int] = {
    weightOfSide(experiment._1) zip
      weightOfSide(experiment._2) map
      { case (leftWeight, rightWeight) => scala.math.signum(leftWeight - rightWeight) }
  }                                               //> signature: (experiment: (Set[Char], Set[Char]))IndexedSeq[Int]

  // Now we filter for solutions
  // Each of the IndexedSeqOfAllCounterfeitsCombos must produce a distinct signature
  def isSolutions(IndexedSeqOfExps: IndexedSeq[(Set[Char], Set[Char])]): Boolean = {
    score(IndexedSeqOfExps) == 0
  }                                               //> isSolutions: (IndexedSeqOfExps: IndexedSeq[(Set[Char], Set[Char])])Boolean

  def score(IndexedSeqOfExps: IndexedSeq[(Set[Char], Set[Char])]): Int = {
    val signatures = (IndexedSeqOfExps map signature).transpose
    // Are they all distinct?
    signatures.size - signatures.distinct.size
  }                                               //> score: (IndexedSeqOfExps: IndexedSeq[(Set[Char], Set[Char])])Int

  def betterScore(IndexedSeqOfExps: IndexedSeq[(Set[Char], Set[Char])]): Int = {
    val signatures = (IndexedSeqOfExps map signature).transpose
    
    signatures.groupBy { identity }.mapValues { _.size }.values.max
  
  }                                               //> betterScore: (IndexedSeqOfExps: IndexedSeq[(Set[Char], Set[Char])])Int

  weightOfSide(Set('A', 'B', 'C'))                //> res0: IndexedSeq[Int] = Vector(3, 3, 2, 3, 3, 2, 2, 3, 3, 3, 3, 2, 3, 3, 2,
                                                  //|  2, 3, 3, 3, 2, 3, 3, 2, 2, 3, 3, 3, 2, 2, 1, 1, 2, 2, 2, 3, 2, 2, 3, 3, 3,
                                                  //|  2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3)
  IndexedSeqOfAllCounterfeitsCombos map { counterfeits =>
    Set('A', 'B', 'C').toIndexedSeq.map { weight => if (counterfeits.contains(weight)) 0 else 1 }
                                                  //> res1: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Inde
                                                  //| xedSeq[Int]] = Vector(Vector(1, 1, 1), Vector(1, 1, 1), Vector(0, 1, 1), Ve
                                                  //| ctor(1, 1, 1), Vector(1, 1, 1), Vector(1, 0, 1), Vector(1, 1, 0), Vector(1,
                                                  //|  1, 1), Vector(1, 1, 1), Vector(1, 1, 1), Vector(1, 1, 1), Vector(0, 1, 1),
                                                  //|  Vector(1, 1, 1), Vector(1, 1, 1), Vector(1, 0, 1), Vector(1, 1, 0), Vector
                                                  //| (1, 1, 1), Vector(1, 1, 1), Vector(1, 1, 1), Vector(0, 1, 1), Vector(1, 1, 
                                                  //| 1), Vector(1, 1, 1), Vector(1, 0, 1), Vector(1, 1, 0), Vector(1, 1, 1), Vec
                                                  //| tor(1, 1, 1), Vector(1, 1, 1), Vector(0, 1, 1), Vector(0, 1, 1), Vector(0, 
                                                  //| 0, 1), Vector(0, 1, 0), Vector(0, 1, 1), Vector(0, 1, 1), Vector(0, 1, 1), 
                                                  //| Vector(1, 1, 1), Vector(1, 0, 1), Vector(1, 1, 0), Vector(1, 1, 1), Vector(
                                                  //| 1, 1, 1), Vector(1, 1, 1), Vector(1, 0, 1), Vector(1, 1, 0), Vector(1, 1, 1
                                                  //| ), Vector(1, 1, 1), Vector(1, 1, 1), Vector(1, 0, 0), Vector(1, 0, 1), Vect
                                                  //| or(1, 0, 1), Vector(1, 
                                                  //| Output exceeds cutoff limit.
  }
  IndexedSeqOfAllCounterfeitsCombos map { counterfeits =>
    Set('A', 'B', 'C').toIndexedSeq.map { weight => if (counterfeits.contains(weight)) 0 else 1 }.sum
                                                  //> res2: scala.collection.immutable.IndexedSeq[Int] = Vector(3, 3, 2, 3, 3, 2,
                                                  //|  2, 3, 3, 3, 3, 2, 3, 3, 2, 2, 3, 3, 3, 2, 3, 3, 2, 2, 3, 3, 3, 2, 2, 1, 1,
                                                  //|  2, 2, 2, 3, 2, 2, 3, 3, 3, 2, 2, 3, 3, 3, 1, 2, 2, 2, 2, 2, 2, 3, 3, 3)
  }
  signature((Set('A', 'B', 'C'), Set('D', 'E', 'F')))
                                                  //> res3: IndexedSeq[Int] = Vector(1, 1, 0, 1, 1, 0, 0, 1, 1, 1, 1, -1, 0, 0, -
                                                  //| 1, -1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 1, -1, -1, -1, -1, -1, -1, 0, 0, -1, -
                                                  //| 1, 0, 0, 1, -1, -1, 0, 0, 1, -1, -1, -1, 0, -1, -1, 0, 0, 1, 1)
  signature((Set('A', 'B', 'C'), Set('C', 'B', 'A')))
                                                  //> res4: IndexedSeq[Int] = Vector(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
                                                  //|  0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
  Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature
                                                  //> res5: scala.collection.immutable.IndexedSeq[IndexedSeq[Int]] = Vector(Vecto
                                                  //| r(-1, 0, -1, 1, 0, -1, 1, 1, 0, 0, -1, -1, 0, -1, -1, 0, 0, -1, -1, -1, 1, 
                                                  //| 0, -1, 1, 1, 0, 0, 0, -1, -1, 0, 0, -1, -1, 1, 0, 1, 1, 1, 1, -1, 1, 1, 0, 
                                                  //| 0, 0, 0, -1, -1, 1, 1, 1, 1, 1, 0), Vector(1, 0, 0, 1, 0, 1, 0, 1, 1, 1, -1
                                                  //| , -1, 0, -1, 0, -1, 1, 1, 1, -1, -1, -1, -1, -1, 0, 0, 0, -1, -1, -1, -1, 0
                                                  //| , 0, 0, -1, 0, -1, 1, 1, 1, -1, -1, 0, 0, 0, -1, 1, 1, 1, 0, 0, 0, 1, 1, 1)
                                                  //| , Vector(1, 1, 0, 0, 0, 1, 1, 0, 1, 1, 0, -1, -1, -1, 1, 1, -1, 1, 0, -1, -
                                                  //| 1, -1, 1, 1, -1, 1, 0, -1, -1, 0, 0, -1, 0, -1, -1, 0, 0, -1, 0, -1, 0, 0, 
                                                  //| -1, 0, -1, 1, 0, 1, 1, 0, 1, 1, 0, -1, 1))

  // We've got a 4X55 matrix.
  // It will be a solution if each of the 55 4X1 columns are unique!
  // First step is transpose the matrix.
  Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature transpose
                                                  //> res6: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Inde
                                                  //| xedSeq[Int]] = Vector(Vector(-1, 1, 1), Vector(0, 0, 1), Vector(-1, 0, 0), 
                                                  //| Vector(1, 1, 0), Vector(0, 0, 0), Vector(-1, 1, 1), Vector(1, 0, 1), Vector
                                                  //| (1, 1, 0), Vector(0, 1, 1), Vector(0, 1, 1), Vector(-1, -1, 0), Vector(-1, 
                                                  //| -1, -1), Vector(0, 0, -1), Vector(-1, -1, -1), Vector(-1, 0, 1), Vector(0, 
                                                  //| -1, 1), Vector(0, 1, -1), Vector(-1, 1, 1), Vector(-1, 1, 0), Vector(-1, -1
                                                  //| , -1), Vector(1, -1, -1), Vector(0, -1, -1), Vector(-1, -1, 1), Vector(1, -
                                                  //| 1, 1), Vector(1, 0, -1), Vector(0, 0, 1), Vector(0, 0, 0), Vector(0, -1, -1
                                                  //| ), Vector(-1, -1, -1), Vector(-1, -1, 0), Vector(0, -1, 0), Vector(0, 0, -1
                                                  //| ), Vector(-1, 0, 0), Vector(-1, 0, -1), Vector(1, -1, -1), Vector(0, 0, 0),
                                                  //|  Vector(1, -1, 0), Vector(1, 1, -1), Vector(1, 1, 0), Vector(1, 1, -1), Vec
                                                  //| tor(-1, -1, 0), Vector(1, -1, 0), Vector(1, 0, -1), Vector(0, 0, 0), Vector
                                                  //| (0, 0, -1), Vector(0, -
                                                  //| Output exceeds cutoff limit.

  (Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature transpose).size
                                                  //> res7: Int = 55
  (Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature transpose).distinct
                                                  //> res8: scala.collection.immutable.IndexedSeq[scala.collection.immutable.Inde
                                                  //| xedSeq[Int]] = Vector(Vector(-1, 1, 1), Vector(0, 0, 1), Vector(-1, 0, 0), 
                                                  //| Vector(1, 1, 0), Vector(0, 0, 0), Vector(1, 0, 1), Vector(0, 1, 1), Vector(
                                                  //| -1, -1, 0), Vector(-1, -1, -1), Vector(0, 0, -1), Vector(-1, 0, 1), Vector(
                                                  //| 0, -1, 1), Vector(0, 1, -1), Vector(-1, 1, 0), Vector(1, -1, -1), Vector(0,
                                                  //|  -1, -1), Vector(-1, -1, 1), Vector(1, -1, 1), Vector(1, 0, -1), Vector(0, 
                                                  //| -1, 0), Vector(-1, 0, -1), Vector(1, -1, 0), Vector(1, 1, -1), Vector(0, 1,
                                                  //|  0), Vector(1, 0, 0))
  (Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature transpose).distinct.size
                                                  //> res9: Int = 25

(Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature transpose)
.groupBy { identity }                             //> res10: scala.collection.immutable.Map[scala.collection.immutable.IndexedSeq
                                                  //| [Int],scala.collection.immutable.IndexedSeq[scala.collection.immutable.Inde
                                                  //| xedSeq[Int]]] = Map(Vector(1, 0, -1) -> Vector(Vector(1, 0, -1), Vector(1, 
                                                  //| 0, -1)), Vector(-1, 0, -1) -> Vector(Vector(-1, 0, -1)), Vector(1, 0, 1) ->
                                                  //|  Vector(Vector(1, 0, 1), Vector(1, 0, 1), Vector(1, 0, 1)), Vector(0, 0, 0)
                                                  //|  -> Vector(Vector(0, 0, 0), Vector(0, 0, 0), Vector(0, 0, 0), Vector(0, 0, 
                                                  //| 0)), Vector(-1, 1, 0) -> Vector(Vector(-1, 1, 0)), Vector(1, -1, -1) -> Vec
                                                  //| tor(Vector(1, -1, -1), Vector(1, -1, -1)), Vector(-1, 1, 1) -> Vector(Vecto
                                                  //| r(-1, 1, 1), Vector(-1, 1, 1), Vector(-1, 1, 1), Vector(-1, 1, 1), Vector(-
                                                  //| 1, 1, 1)), Vector(0, 1, 0) -> Vector(Vector(0, 1, 0)), Vector(0, 1, 1) -> V
                                                  //| ector(Vector(0, 1, 1), Vector(0, 1, 1), Vector(0, 1, 1)), Vector(1, -1, 0) 
                                                  //| -> Vector(Vector(1, -1, 0), Vector(1, -1, 0)), Vector(-1, 0, 0) -> Vector(V
                                                  //| ector(-1, 0, 0), Vector
                                                  //| Output exceeds cutoff limit.

(Set((Set('J', 'A', 'B'), Set('I', 'C', 'H')), (Set('F', 'A', 'G', 'C'),
    Set('E', 'H', 'K', 'D')), (Set('A', 'I', 'G', 'H'), Set('E', 'B', 'C', 'K'))).toIndexedSeq map signature transpose)
.groupBy { identity }.mapValues { _.size }.values.max
                                                  //> res11: Int = 5

// We have a 12826 x 55 matrix. 12826 experiments, each with a signature of 55 results based on the cointerfeit choice.
// We need to select 4 rows of the 12826 such that we get a 4x55 matrix such that each 4x1 column vector is unique.
// Since each element has only 3 possible values, {-1,0,1}, there is a total of 81 (3^4) unique column vectors of size 4. So it should be possible.
// What does it mean that they are all unique? Is that something like an orthonormal basis set???
// It means that if we have a solution, 4 rows... then any 3 of those rows will get us mostly there. Adding that last row can't fix
// huge problems. A single experiment can only discriminate between 3 cases since there are only 3 outcomes.


  // How about if we assume -- without loss of generality -- that one of the experiments is a 4 vs 4, namely...
  // OMG! What a bad assuption. It turns out that there are no solutions where any of the experiments are 4 vs 4!!
  // There are 4 solutions with 1 5v5 and 3 3v3, and 4 solutions with all 4 3v3.

  val experiment1 = (Set('A', 'B', 'C', 'D','E'), Set('F', 'G', 'H','I','J'))
                                                  //> experiment1  : (scala.collection.immutable.Set[Char], scala.collection.immu
                                                  //| table.Set[Char]) = (Set(E, A, B, C, D),Set(J, F, I, G, H))

  // Then the second experiment is preferentially chosen such that it helps discriminate the cases the most.
  // Same with the third
  // The fourth, we are doing O(n) anyway on checking for solutions, so there's no point in sorting.
  for {
    experiment2 <- experiments.sortBy(experiment2 => betterScore(IndexedSeq(experiment1, experiment2))).filter(
      experiment2 => betterScore(IndexedSeq(experiment1, experiment2)) <= 9)
    experiment3 <- experiments.sortBy(experiment3 => betterScore(IndexedSeq(experiment1, experiment2, experiment3))).filter(
      experiment3 => betterScore(IndexedSeq(experiment1, experiment2, experiment3)) <= 3)
    experiment4 <- experiments
    if isSolutions(IndexedSeq(experiment1, experiment2, experiment3, experiment4))
  } println(experiment1, experiment2, experiment3, experiment4)
                                                  //> ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(F
                                                  //| , G, C),Set(I, H, D)),(Set(A, G, D),Set(E, C, H)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(F
                                                  //| , G, C),Set(I, H, D)),(Set(A, C, H),Set(E, G, D)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(F
                                                  //| , C, H),Set(I, G, D)),(Set(A, G, C),Set(E, H, D)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(F
                                                  //| , C, H),Set(I, G, D)),(Set(A, H, D),Set(E, G, C)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(A
                                                  //| , G, C),Set(E, H, D)),(Set(F, C, H),Set(I, G, D)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(A
                                                  //| , G, C),Set(E, H, D)),(Set(I, C, H),Set(F, G, D)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G, H)),(Set(E, J, A),Set(F, I, B)),(Set(A
                                                  //| , G, D),Set(E, C, H)),(Set(F, G, C),Set(I, H, D)))
                                                  //| ((Set(E, A, B, C, D),Set(J, F, I, G,
                                                  //| Output exceeds cutoff limit./

// }
}