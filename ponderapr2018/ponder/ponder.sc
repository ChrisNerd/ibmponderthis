object ponder {
  val MAXBALLOON = 9                              //> MAXBALLOON  : Int = 9
  
// To get the Powerset
 (1 to MAXBALLOON).toSet.subsets.toList           //> res0: List[scala.collection.immutable.Set[Int]] = List(Set(), Set(5), Set(1)
                                                  //| , Set(6), Set(9), Set(2), Set(7), Set(3), Set(8), Set(4), Set(5, 1), Set(5, 
                                                  //| 6), Set(5, 9), Set(5, 2), Set(5, 7), Set(5, 3), Set(5, 8), Set(5, 4), Set(1,
                                                  //|  6), Set(1, 9), Set(1, 2), Set(1, 7), Set(1, 3), Set(1, 8), Set(1, 4), Set(6
                                                  //| , 9), Set(6, 2), Set(6, 7), Set(6, 3), Set(6, 8), Set(6, 4), Set(9, 2), Set(
                                                  //| 9, 7), Set(9, 3), Set(9, 8), Set(9, 4), Set(2, 7), Set(2, 3), Set(2, 8), Set
                                                  //| (2, 4), Set(7, 3), Set(7, 8), Set(7, 4), Set(3, 8), Set(3, 4), Set(8, 4), Se
                                                  //| t(5, 1, 6), Set(5, 1, 9), Set(5, 1, 2), Set(5, 1, 7), Set(5, 1, 3), Set(5, 1
                                                  //| , 8), Set(5, 1, 4), Set(5, 6, 9), Set(5, 6, 2), Set(5, 6, 7), Set(5, 6, 3), 
                                                  //| Set(5, 6, 8), Set(5, 6, 4), Set(5, 9, 2), Set(5, 9, 7), Set(5, 9, 3), Set(5,
                                                  //|  9, 8), Set(5, 9, 4), Set(5, 2, 7), Set(5, 2, 3), Set(5, 2, 8), Set(5, 2, 4)
                                                  //| , Set(5, 7, 3), Set(5, 7, 8), Set(5, 7, 4), Set(5, 3, 8), Set(5, 3, 4), Set(
                                                  //| 5, 8, 4), Set(1, 6, 9), 
                                                  //| Output exceeds cutoff limit.
  (1 to MAXBALLOON).toSet.subsets.toList.filter(_.sum < MAXBALLOON + .5)
                                                  //> res1: List[scala.collection.immutable.Set[Int]] = List(Set(), Set(5), Set(1)
                                                  //| , Set(6), Set(9), Set(2), Set(7), Set(3), Set(8), Set(4), Set(5, 1), Set(5, 
                                                  //| 2), Set(5, 3), Set(5, 4), Set(1, 6), Set(1, 2), Set(1, 7), Set(1, 3), Set(1,
                                                  //|  8), Set(1, 4), Set(6, 2), Set(6, 3), Set(2, 7), Set(2, 3), Set(2, 4), Set(3
                                                  //| , 4), Set(5, 1, 2), Set(5, 1, 3), Set(1, 6, 2), Set(1, 2, 3), Set(1, 2, 4), 
                                                  //| Set(1, 3, 4), Set(2, 3, 4))
  (1 to MAXBALLOON).toSet.subsets.toList.filter(_.sum < MAXBALLOON + .5).size
                                                  //> res2: Int = 33
  // Maybe we should get rid of the empty set
  // Yes. Also get rid of the sets with just 1 entry since that gives no descrestionary power to Bob
  (1 to MAXBALLOON).toSet.subsets.toList.filter(_.sum < MAXBALLOON + .5).filter(_.size >= 2)
                                                  //> res3: List[scala.collection.immutable.Set[Int]] = List(Set(5, 1), Set(5, 2),
                                                  //|  Set(5, 3), Set(5, 4), Set(1, 6), Set(1, 2), Set(1, 7), Set(1, 3), Set(1, 8)
                                                  //| , Set(1, 4), Set(6, 2), Set(6, 3), Set(2, 7), Set(2, 3), Set(2, 4), Set(3, 4
                                                  //| ), Set(5, 1, 2), Set(5, 1, 3), Set(1, 6, 2), Set(1, 2, 3), Set(1, 2, 4), Set
                                                  //| (1, 3, 4), Set(2, 3, 4))
  
  
  (1 to MAXBALLOON).toSet.subsets.toList.filter(_.sum < MAXBALLOON + .5).filter(_.size >= 2).size
                                                  //> res4: Int = 23
  
  val presentations = (1 to MAXBALLOON).toSet.subsets.filter(_.sum < MAXBALLOON + .5).filter(_.size >= 2).toSet.subsets
                                                  //> presentations  : Iterator[scala.collection.immutable.Set[scala.collection.im
                                                  //| mutable.Set[Int]]] = non-empty iterator
  def bobDiscriminator(alice: Set[Set[Int]]): Boolean = {
  /// We're looking for if all the permutations, 1 maps to 1 are the only ones that pass all aliceTests
  // We're looking for if 1 maps to anything else, it shouldn't pass at least one of the tests.
  (1 to MAXBALLOON).permutations.forall(bobCandidate => bobCandidate(0) == 1 ||  alice.exists {
    aliceTest => aliceTest.map(i => bobCandidate(i-1)).sum > MAXBALLOON + .5 })
  
  }                                               //> bobDiscriminator: (alice: Set[Set[Int]])Boolean
  presentations.find( bobDiscriminator )          //> res5: Option[scala.collection.immutable.Set[scala.collection.immutable.Set[
                                                  //| Int]]] = Some(Set(Set(5, 1, 3), Set(5, 4), Set(1, 6, 2), Set(1, 3, 4)))
// Solution to N=6
// Some(Set(Set(5, 1), Set(1, 4), Set(1, 2, 3)))

// Solution to N=9
//  Some(Set(Set(5, 1, 3), Set(5, 4), Set(1, 6, 2), Set(1, 3, 4)))
}