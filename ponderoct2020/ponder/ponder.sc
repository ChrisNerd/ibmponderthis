object ponder {


 ('a' to 'c').toList.combinations(2).foreach(println)
                                                  //> List(a, b)
                                                  //| List(a, c)
                                                  //| List(b, c)

  
  
  val start = List('a','b')                       //> start  : List[Char] = List(a, b)
  
  val rules: Map[Char, List[Char]] = Map(
  'a' -> List('b','d','z'),
  'b' -> List('a','d','z'),
  'c' -> List('b','d','z'),
  'd' -> List('b','c','e','f'),
  'e' -> List('c','z'),
  'f' -> List('b','c','z'),
  'z' -> Nil
  )                                               //> rules  : Map[Char,List[Char]] = Map(e -> List(c, z), f -> List(b, c, z), a -
                                                  //| > List(b, d, z), b -> List(a, d, z), c -> List(b, d, z), z -> List(), d -> L
                                                  //| ist(b, c, e, f))
  
/*  def r( a : List[Char])  : List[Char] =
  {
  ...
  }
  */
  
 val bonusRules = Map('e' -> List('c', 'd', 'f'), 'f' -> List(), 'a' -> List('a'),
     'b' -> List('c'), 'c' -> List( 'c', 'd','e'), 'z' -> List(), 'd' -> List('c', 'd','f'))
                                                  //> bonusRules  : scala.collection.immutable.Map[Char,List[Char]] = Map(e -> Lis
                                                  //| t(c, d, f), f -> List(), a -> List(a), b -> List(c), c -> List(c, d, e), z -
                                                  //| > List(), d -> List(c, d, f))

  (0 to 12).scanLeft(start)( (a,b) => a.flatMap(bonusRules))
  .map( _.count(_ == 'z'))                        //> res0: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 0, 0, 0, 0, 0, 
                                                  //| 0, 0, 0, 0, 0, 0, 0, 0)
  (0 to 12).scanLeft(start)( (a,b) => a.flatMap(bonusRules))
  .map( _.count(_ == 'f'))                        //> res1: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 0, 0, 2, 4, 10,
                                                  //|  24, 58, 140, 338, 816, 1970, 4756, 11482)
                                                  
                                                  
  
  val l = (0 to 6).scanLeft(start)( (a,b) => a.flatMap(rules))
                                                  //> l  : scala.collection.immutable.IndexedSeq[List[Char]] = Vector(List(a, b), 
                                                  //| List(b, d, z, a, d, z), List(a, d, z, b, c, e, f, b, d, z, b, c, e, f), List
                                                  //| (b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, 
                                                  //| a, d, z, b, d, z, c, z, b, c, z), List(a, d, z, b, c, e, f, a, d, z, b, d, z
                                                  //| , c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z,
                                                  //|  b, d, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, 
                                                  //| c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z), List(b, d, z, b, c
                                                  //| , e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c,
                                                  //|  e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, 
                                                  //| z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b
                                                  //| , c, e, f, b, d, z, b, c, e, f, a, d, z, b, c, e, f, a, d, z, b, c, e, f, a,
                                                  //|  d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, 
                                                  //| d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z
                                                  //| , b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f,
                                                  //|  b, d, z, b, c, e, f, a, d, z, b, c, e, f), List(a, d, z, b, c, e, f, a, d, 
                                                  //| z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z
                                                  //| , a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b,
                                                  //|  d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, b, 
                                                  //| d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b, d, z
                                                  //| , c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z,
                                                  //|  b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, 
                                                  //| c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, b, d, z, b, c, e, f
                                                  //| , a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c,
                                                  //|  z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, 
                                                  //| b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b
                                                  //| , c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z,
                                                  //|  c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, 
                                                  //| z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, b, c, e, f
                                                  //| , a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f,
                                                  //|  b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, 
                                                  //| c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, b
                                                  //| , d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, a,
                                                  //|  d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, 
                                                  //| b, c, z), List(b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z
                                                  //| , b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c,
                                                  //|  e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, 
                                                  //| z, c, z, b, c, z, a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b, c, e
                                                  //| , f, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e,
                                                  //|  f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, 
                                                  //| d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d
                                                  //| , z, a, d, z, b, d, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z,
                                                  //|  a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, 
                                                  //| a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z
                                                  //| , b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d,
                                                  //|  z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, 
                                                  //| f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, b, d, z, b, c, e, f
                                                  //| , a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z,
                                                  //|  b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, 
                                                  //| z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z
                                                  //| , b, d, z, c, z, b, c, z, a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z,
                                                  //|  b, c, e, f, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, 
                                                  //| b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, b, d, z, b, c, e
                                                  //| , f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e,
                                                  //|  f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, 
                                                  //| b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z
                                                  //| , a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f,
                                                  //|  a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, 
                                                  //| z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c
                                                  //| , z, a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z,
                                                  //|  b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, 
                                                  //| b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z
                                                  //| , c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z,
                                                  //|  b, d, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, a, d, z, b, 
                                                  //| c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d
                                                  //| , z, c, z, b, c, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b,
                                                  //|  d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, 
                                                  //| b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b
                                                  //| , d, z, c, z, b, c, z, a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b,
                                                  //|  c, e, f, b, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, 
                                                  //| c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, a, d, z, b, c, e, f
                                                  //| , a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, d, z, c,
                                                  //|  z, b, c, z, a, d, z, b, c, e, f, b, d, z, b, c, e, f, a, d, z, b, c, e, f, 
                                                  //| a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a
                                                  //| , d, z, b, c, e, f, b, d, z, a, d, z, b, d, z, b, d, z, b, c, e, f, a, d, z,
                                                  //|  b, d, z, c, z, b, c, z, b, d, z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, 
                                                  //| a, d, z, b, d, z, a, d, z, b, c, e, f, a, d, z, b, d, z, c, z, b, c, z, b, d
                                                  //| , z, b, c, e, f, a, d, z, b, c, e, f, b, d, z, a, d, z, b, d, z))
  
  (0 to 6).map(x => if (x%2==0) scala.math.pow(2, x) +1  else scala.math.pow(2,x)).map(_.toInt)
                                                  //> res2: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 2, 5, 8, 17, 3
                                                  //| 2, 65)
  
  l.map( _.count(_ == 'z'))                       //> res3: scala.collection.immutable.IndexedSeq[Int] = Vector(0, 2, 2, 10, 20, 
                                                  //| 46, 116, 268)
                                                  
  
                                                  
 
  val sol = for {
   an <- (1 to 3).toIterator
   a <- ('a' to 'f').toList.combinations(an).map( l => (l.toSet /* taking out the direct path to f for the bonus question+ 'f'*/).toList).toIterator
   bn <- 1 to 3
   b <- ('a' to 'f').toList.combinations(bn).map( l => (l.toSet /*+ 'f'*/).toList)
   cn <- 1 to 3
   c <- ('a' to 'f').toList.combinations(cn)
   dn <- 1 to 3
   d <- ('a' to 'f').toList.combinations(dn)
   en <- 0 to 3
   e <- ('a' to 'f').toList.combinations(en)
   val rules: Map[Char, List[Char]] = Map(
    'a' -> a,
    'b' -> b,
    'c' -> c,
    'd' -> d,
    'e' -> e,
    'f' -> Nil,
    'z' -> Nil
   )
   
  // val l = (0 to 15).toIterator.scanLeft(start)( (a,b) => a.flatMap(rules))
   // for the bonus
   // if (l.map(_.count(_ =='f)).takeWhile(_ < 10000).contains(1970))
   
   // for the non-bonus
   //if ( l.map(_.count(_ =='f')) == Vector(0,2,2,5,8,17,32,65))
   
   // no filter if we're going to use matrices
  }yield rules // (a,b,c,d,e)                     //> sol  : Iterator[scala.collection.immutable.Map[Char,List[Char]]] = non-empt
                                                  //| y iterator

 def multiply(m1: IndexedSeq[List[Int]], m2: IndexedSeq[List[Int]]) = {
  val s = m1.size
  val test = (0 until s).toList
  
  for {
   r1 <- m1.zipWithIndex
  } yield { for { c <- (0 until s).toList} yield {
  val col = (0 until s).toList.map(r2 => m2(r2)(c))
   col.zip(r1._1).map{ case (a,b) => a*b}.sum
  }
  }
 }                                                //> multiply: (m1: IndexedSeq[List[Int]], m2: IndexedSeq[List[Int]])IndexedSeq[
                                                  //| List[Int]]
 
 multiply(IndexedSeq( List(1,2,3), List(4,4,6), List(8,2,1)), IndexedSeq(List( 9,2,3),List( 2 ,6,9), List(8,5,1)))
                                                  //> res4: IndexedSeq[List[Int]] = Vector(List(37, 29, 24), List(92, 62, 54), Li
                                                  //| st(84, 33, 43))
 // Verified by Octave
             /* octave:1> a= [[1,2,3];[4,4,6];[8,2,1]]
a =

   1   2   3
   4   4   6
   8   2   1

octave:2> b=[[9,2,3];[2,6,9];[8,5,1]]
b =

   9   2   3
   2   6   9
   8   5   1

octave:3> a*b
ans =

   37   29   24
   92   62   54
   84   33   43
              */


 def multiplyMatColumn(m1: IndexedSeq[List[Int]], m2: IndexedSeq[Int]) = {
  for {
   r1 <- m1
  } yield {
   m2.zip(r1).map{ case (a,b) => a*b}.sum
  }
 }                                                //> multiplyMatColumn: (m1: IndexedSeq[List[Int]], m2: IndexedSeq[Int])IndexedS
                                                  //| eq[Int]

 def recurs(mRunning: IndexedSeq[Int], m: IndexedSeq[List[Int]], d: Int): Boolean  = {
  if (d==0)
   recurs(IndexedSeq(1,1,0,0,0,0), m, 1)
  else if (d == 7 && mRunning.last == 0 || d > 7 && mRunning.last > 10000)
   false
  else if (d < 20)
  {
  if (mRunning.last == 1970)
   true
  else
//   recurs( multiply(mRunning, m),m, d+1)
 recurs(multiplyMatColumn(m, mRunning), m, d+1)
  }
  else
   false
 }                                                //> recurs: (mRunning: IndexedSeq[Int], m: IndexedSeq[List[Int]], d: Int)Boolea
                                                  //| n

  val r2 = sol.drop(10000).next                   //> r2  : scala.collection.immutable.Map[Char,List[Char]] = Map(e -> List(d), f
                                                  //|  -> List(), a -> List(a), b -> List(a), c -> List(f), z -> List(), d -> Lis
                                                  //| t(b, c, f))
  val matr2 = ('a' to 'f').map{ charStart =>
   ('a' to 'f').toList.map(charEnd => if (r2(charEnd).contains(charStart)) 1 else 0  )
   }                                              //> matr2  : scala.collection.immutable.IndexedSeq[List[Int]] = Vector(List(1, 
                                                  //| 1, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 
                                                  //| 0, 0, 1, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 1, 0, 0))
 val m3 = multiplyMatColumn(matr2, IndexedSeq(1,1,0,0,0,0))
                                                  //> m3  : IndexedSeq[Int] = Vector(2, 0, 0, 0, 0, 0)
 multiplyMatColumn(matr2, m3)                     //> res5: IndexedSeq[Int] = Vector(2, 0, 0, 0, 0, 0)

 val sol2 = sol.foreach{ r =>
  val matr = ('a' to 'f').map{ charStart =>
   ('a' to 'f').toList.map(charEnd => if (r(charEnd).contains(charStart)) 1 else 0  )
  }
  if (recurs(IndexedSeq(), matr, 0))
   {
   println("Solution found!")
   println(r)
   println(matr)
  }
 }                                                //> Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(c), c -> List(
                                                  //| c, d, e), z -> List(), d -> List(c, d, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, e, f), f -> List(), a -> List(a), b -> List(c), c -> List(
                                                  //| c, d, e), z -> List(), d -> List(c, d, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(c), c -> List(
                                                  //| c, d, e), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, e, f), f -> List(), a -> List(a), b -> List(c), c -> List(
                                                  //| c, d, e), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(c), c -> List(
                                                  //| d, e, f), z -> List(), d -> List(b, c, d))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 1, 0, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 1, 0, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(b, c, e), f -> List(), a -> List(a), b -> List(c), c -> List(
                                                  //| d, e, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 1, 0, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(d), c -> List(
                                                  //| b, c, d), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 1, 1, 0, 1, 0), List(0, 0, 0, 1, 1, 0), List(0, 0, 0, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(b, d, e), f -> List(), a -> List(a), b -> List(d), c -> List(
                                                  //| c, d, e), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 0, 1, 1, 0, 
                                                  //| 0), List(0, 1, 1, 0, 1, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 0, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(d), c -> List(
                                                  //| c, d, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(d, e, f), f -> List(), a -> List(a), b -> List(d), c -> List(
                                                  //| c, d, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 1, 0, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 0, 1, 1, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(d), c -> List(
                                                  //| d, e, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 1, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(d, e, f), f -> List(), a -> List(a), b -> List(d), c -> List(
                                                  //| d, e, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(e), c -> List(
                                                  //| b, c, e), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 0, 0, 1, 1, 0), List(0, 1, 1, 1, 0, 0), List(0, 0, 0, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(e), c -> List(
                                                  //| c, d, e), z -> List(), d -> List(b, d, e))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 1, 0, 1, 
                                                  //| 0), List(0, 0, 1, 1, 1, 0), List(0, 1, 1, 1, 0, 0), List(0, 0, 0, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(e), c -> List(
                                                  //| c, e, f), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 0, 0, 0, 1, 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(e), c -> List(
                                                  //| c, e, f), z -> List(), d -> List(d, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 0, 1, 
                                                  //| 0), List(0, 0, 0, 1, 1, 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(e), c -> List(
                                                  //| d, e, f), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 1, 
                                                  //| 0), List(0, 0, 1, 0, 1, 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(e), c -> List(
                                                  //| d, e, f), z -> List(), d -> List(d, e, f))
                                                  //| Vector(List(1, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 0, 1, 
                                                  //| 0), List(0, 0, 1, 1, 1, 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(a, c), c -> Li
                                                  //| st(c, d, e), z -> List(), d -> List(c, d, f))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, e, f), f -> List(), a -> List(a), b -> List(a, c), c -> Li
                                                  //| st(c, d, e), z -> List(), d -> List(c, d, f))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(a, c), c -> Li
                                                  //| st(c, d, e), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, e, f), f -> List(), a -> List(a), b -> List(a, c), c -> Li
                                                  //| st(c, d, e), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 1, 1, 1, 1, 
                                                  //| 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 0, 1, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(a, c), c -> Li
                                                  //| st(d, e, f), z -> List(), d -> List(b, c, d))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 0), List(0, 1, 0, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 1, 0), List(0, 0, 1, 0, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(b, c, e), f -> List(), a -> List(a), b -> List(a, c), c -> Li
                                                  //| st(d, e, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 1, 0, 1, 1, 
                                                  //| 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, e), f -> List(), a -> List(a), b -> List(a, d), c -> Li
                                                  //| st(b, c, d), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 1, 1, 0, 1, 0), List(0, 0, 0, 1, 1, 0), List(0, 0, 0, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(b, d, e), f -> List(), a -> List(a), b -> List(a, d), c -> Li
                                                  //| st(c, d, e), z -> List(), d -> List(c, e, f))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 1, 0), List(0, 0, 1, 1, 0, 
                                                  //| 0), List(0, 1, 1, 0, 1, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 0, 1, 0, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(a, d), c -> Li
                                                  //| st(c, d, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 0, 1, 0, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(d, e, f), f -> List(), a -> List(a), b -> List(a, d), c -> Li
                                                  //| st(c, d, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 1, 1, 0, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 0, 1, 1, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(a, d), c -> Li
                                                  //| st(d, e, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 1, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 0, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(d, e, f), f -> List(), a -> List(a), b -> List(a, d), c -> Li
                                                  //| st(d, e, f), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 0, 0, 0, 0), List(0, 0, 0, 1, 0, 
                                                  //| 0), List(0, 1, 1, 1, 1, 0), List(0, 0, 1, 1, 1, 0), List(0, 0, 1, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(a, e), c -> Li
                                                  //| st(b, c, e), z -> List(), d -> List(c, d, e))
                                                  //| Vector(List(1, 1, 0, 0, 0, 0), List(0, 0, 1, 0, 0, 0), List(0, 0, 1, 1, 1, 
                                                  //| 0), List(0, 0, 0, 1, 1, 0), List(0, 1, 1, 1, 0, 0), List(0, 0, 0, 0, 1, 0))
                                                  //| 
                                                  //| Solution found!
                                                  //| Map(e -> List(c, d, f), f -> List(), a -> List(a), b -> List(a, e), c -> Li


 sol.foreach(println)

 println("here1")
                                                  
  
}