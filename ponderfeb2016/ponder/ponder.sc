object ponder {
  
/*
a**connect = c (mod inter)
a**connect = a**toe (mod at)
a**connect = a**toe (mod cm)
n**i = ban*/

/*
aconetirbm
0123456789
*/
  
val solutions = for {IndexedSeq(a,c,o,n,e,t,i,r,b,m) <- (0 to 9).permutations
if (
BigInt(a).modPow(c*1000000+o*100000+n*10000+n*1000+e*100+c*10+t, i*10000+n*1000+t*100+e*10+r) == c &&
BigInt(a).modPow(c*1000000+o*100000+n*10000+n*1000+e*100+c*10+t, a*10+t) == BigInt(a).modPow(t*100+o*10+e, a*10+t) &&
BigInt(a).modPow(c*1000000+o*100000+n*10000+n*1000+e*100+c*10+t, c*10+m) == BigInt(a).modPow(t*100+o*10+e, c*10+m) &&
math.pow(n,i) == 100*b+10*a+n
)
}
yield IndexedSeq(a,c,o,n,e,t,i,r,b,m)             //> solutions  : Iterator[IndexedSeq[Int]] = non-empty iterator

//solutions foreach println
val solMap = List("a","c","o","n","e","t","i","r","b","m") zip solutions.toList.head //foreach println(_.2 "=" ._1)
                                                  //> solMap  : List[(String, Int)] = List((a,2), (c,8), (o,1), (n,5), (e,0), (t,3
                                                  //| ), (i,4), (r,7), (b,6), (m,9))

"connect" map (let => (solMap.filter(x => x._1 == let.toString)).head._2 )
                                                  //> res0: scala.collection.immutable.IndexedSeq[Int] = Vector(8, 1, 5, 5, 0, 8, 
                                                  //| 3)
"inter" map (let => (solMap.filter(x => x._1 == let.toString)).head._2 )
                                                  //> res1: scala.collection.immutable.IndexedSeq[Int] = Vector(4, 5, 3, 0, 7)
"toe" map (let => (solMap.filter(x => x._1 == let.toString)).head._2 )
                                                  //> res2: scala.collection.immutable.IndexedSeq[Int] = Vector(3, 1, 0)
"at" map (let => (solMap.filter(x => x._1 == let.toString)).head._2 )
                                                  //> res3: scala.collection.immutable.IndexedSeq[Int] = Vector(2, 3)
"cm" map (let => (solMap.filter(x => x._1 == let.toString)).head._2 )
                                                  //> res4: scala.collection.immutable.IndexedSeq[Int] = Vector(8, 9)
"ban" map (let => (solMap.filter(x => x._1 == let.toString)).head._2 )
                                                  //> res5: scala.collection.immutable.IndexedSeq[Int] = Vector(6, 2, 5)


BigInt(2).modPow(8155083,45307) == 8              //> res6: Boolean = true
BigInt(2).modPow(8155083,23) == BigInt(2).modPow(310, 23)
                                                  //> res7: Boolean = true
BigInt(2).modPow(8155083,89) == BigInt(2).modPow(310, 89)
                                                  //> res8: Boolean = true
math.pow(5,4) == 625                              //> res9: Boolean = true

BigInt(2).modPow(8155083,23)                      //> res10: scala.math.BigInt = 4
BigInt(2).modPow(8155083,89)                      //> res11: scala.math.BigInt = 4
}