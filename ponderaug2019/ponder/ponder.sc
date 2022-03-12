object ponder {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  /*Given 9 letters A,B,C,...I , there are 9! = 362,880 different ways to sort them.
Imposing some restrictions, like B>C, C>D, and E>G makes the space smaller (30,240 in this case).

Under a restriction, some of the 36 pairs are determined (B>D, in the example above), and some could be valid in both directions (A>G and G>A are both possible).
In this example we can choose two permutations A<D<C<B<F<G<E<H<I and I<H<G<E<F<D<C<B<A) that cover all the possibilities for the undetermined pairs.
Your challenge, this month, is to find restrictions on 9 letters that is feasible (there exists an order consistent with all restrictions) and no three permutations cover all the pairs.
Supply your answer in the format "B>C, C>D, E>G" and explain why it solves the problem.
*/

val s = ('A' to 'I')                              //> s  : scala.collection.immutable.NumericRange.Inclusive[Char] = NumericRange 
                                                  //| A to I

 s.permutations.size                              //> res0: Int = 362880

 s.permutations.take(10).foreach(println)         //> Vector(A, B, C, D, E, F, G, H, I)
                                                  //| Vector(A, B, C, D, E, F, G, I, H)
                                                  //| Vector(A, B, C, D, E, F, H, G, I)
                                                  //| Vector(A, B, C, D, E, F, H, I, G)
                                                  //| Vector(A, B, C, D, E, F, I, G, H)
                                                  //| Vector(A, B, C, D, E, F, I, H, G)
                                                  //| Vector(A, B, C, D, E, G, F, H, I)
                                                  //| Vector(A, B, C, D, E, G, F, I, H)
                                                  //| Vector(A, B, C, D, E, G, H, F, I)
                                                  //| Vector(A, B, C, D, E, G, H, I, F)

 val restrictions = Set( ('B','C'), ('C','D'), ('E', 'G'))
                                                  //> restrictions  : scala.collection.immutable.Set[(Char, Char)] = Set((B,C), (C
                                                  //| ,D), (E,G))

 s.permutations.filter( p => restrictions.forall(r => p.indexOf(r._1) > p.indexOf(r._2)  )).size
                                                  //> res1: Int = 30240

 s.toSet.subsets(2).size                          //> res2: Int = 36
 s.toSet.subsets(2).take(10).foreach(println)     //> Set(E, F)
                                                  //| Set(E, A)
                                                  //| Set(E, I)
                                                  //| Set(E, G)
                                                  //| Set(E, B)
                                                  //| Set(E, C)
                                                  //| Set(E, H)
                                                  //| Set(E, D)
                                                  //| Set(F, A)
                                                  //| Set(F, I)
                                                  
 // What we need is to look for the following:
 // is to find restrictions on 9 letters that is feasible (there exists an order consistent with all restrictions) and no three permutations cover all the pairs.
 // find restriction Set, restrictions such that:
 
 def setOfPairsFromList( l : IndexedSeq[Char]) : Set[(Char,Char)] = l.combinations(2).map(x => (x(0),x(1))).toSet
                                                  //> setOfPairsFromList: (l: IndexedSeq[Char])Set[(Char, Char)]
 
 s.permutations.filter( p => restrictions.forall(r => p.indexOf(r._1) > p.indexOf(r._2)  )).toSet.subsets(3).forall( threePerms =>  threePerms.flatMap( f => setOfPairsFromList(f)) ==  s.toSet.subsets(2).toSet)
                                                  //> res3: Boolean = false
                                                  
 
}