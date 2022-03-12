object ponder {

 val remaining = "PONDERTHIS".toCharArray().toSet.subsets(5).toSet
                                                  //> remaining  : scala.collection.immutable.Set[scala.collection.immutable.Set[Ch
                                                  //| ar]] = Set(Set(E, N, I, H, R), Set(N, T, I, R, O), Set(E, T, I, O, S), Set(N,
                                                  //|  T, H, R, D), Set(N, T, P, H, S), Set(E, N, T, I, O), Set(E, H, R, O, D), Set
                                                  //| (N, R, O, D, S), Set(E, N, P, R, S), Set(N, T, H, D, S), Set(E, N, P, H, O), 
                                                  //| Set(T, I, H, R, S), Set(N, I, P, H, O), Set(N, I, P, H, R), Set(T, H, R, O, D
                                                  //| ), Set(E, N, T, R, O), Set(E, N, T, I, R), Set(N, I, H, O, D), Set(N, P, H, R
                                                  //| , O), Set(N, T, P, R, O), Set(E, N, I, H, O), Set(E, T, I, O, D), Set(N, T, I
                                                  //| , P, O), Set(N, P, R, D, S), Set(E, N, O, D, S), Set(I, H, R, D, S), Set(N, I
                                                  //| , H, O, S), Set(N, T, I, P, R), Set(N, I, H, R, O), Set(E, N, H, R, O), Set(E
                                                  //| , N, P, H, S), Set(N, P, H, R, D), Set(E, P, H, R, D), Set(E, I, R, D, S), Se
                                                  //| t(E, T, I, P, S), Set(N, T, P, H, O), Set(T, P, R, D, S), Set(N, T, P, H, R),
                                                  //|  Set(E, N, R, O, D), Set(E, I, P, R, O), Set(E, H, R, O, S), Set(N, T, H, R, 
                                                  //| O), Set(E, N, I, P, H), S
                                                  //| Output exceeds cutoff limit.
 


 def findSolution(stringSoFar: String, remaining: Set[Set[Char]]): Boolean = remaining.size match {
  case 0 => {
  println(stringSoFar)
  true
  }
  case _ => {
   if (stringSoFar.size > 270) false else
   {
    "PONDERTHIS".toCharArray()
     .sortBy{ l =>
     (if (stringSoFar.take(4)(0)  == l ) 10000
     else if (stringSoFar.take(4)(1)  == l ) 9000
     else if (stringSoFar.take(4)(2)  == l ) 8000
     else if (stringSoFar.take(4)(3)  == l ) 7000
     else
     0) +
     remaining.count( _.contains(l)) * 10 +
     remaining.count( p => ( Set(l) + stringSoFar.take(4)(0) ).subsetOf(p) ) * 10 +
     remaining.count( p => ( Set(l) + stringSoFar.take(4)(0) + stringSoFar.take(4)(1)).subsetOf(p) ) * 9 +
     remaining.count( p => ( Set(l) + stringSoFar.take(4)(0) + stringSoFar.take(4)(1) + stringSoFar.take(4)(2)).subsetOf(p)) * 8
     }
     .find(l => findSolution( stringSoFar + l ,
      remaining -- Set( (stringSoFar.take(4) + l).toCharArray().toSet))) match
    {
    case t: Some[Char] => true
    case _ => false
   }
   }
  }
 }                                                //> findSolution: (stringSoFar: String, remaining: Set[Set[Char]])Boolean\
 
 findSolution("PONDE",remaining)
 
}