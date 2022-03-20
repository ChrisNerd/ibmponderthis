import scala.math.BigInt
object ponder {
  def rev(n: BigInt) = BigInt(n.toString.reverse) //> rev: (n: scala.math.BigInt)scala.math.BigInt
  def numDigits(x: BigInt) = x.toString.length    //> numDigits: (x: scala.math.BigInt)Int
  def lastNdigits(x: BigInt, n: Int) = BigInt(x.toString.takeRight(n))
                                                  //> lastNdigits: (x: scala.math.BigInt, n: Int)scala.math.BigInt
  // (BigInt(1) to BigInt(1000000000)) filter (x => x == rev(lastNdigits(x * x, numDigits(x))))
  //filter (x => x == rev(lastNdigits(x * x, numDigits(x))))
  //val a:Stream[BigInt] = Stream from(BigInt(1))
  // val it = Iterator.iterate(BigInt(1))(_ + 1) filter (x => x == rev(lastNdigits(x * x, numDigits(x)))) /*take (5)*/ foreach(i => println(i))

  def constructX(xl: BigInt, q: Int, m: Int): BigInt = {
    val firstString = if (q % 2 != 0)
      rev(lastNdigits(xl * xl, m - 1)).toString
    else
      rev(lastNdigits(xl * xl, m)).toString
    BigInt(firstString ++ "0" * (q - firstString.length - numDigits(xl)) ++ xl.toString)
  }                                               //> constructX: (xl: scala.math.BigInt, q: Int, m: Int)scala.math.BigInt

  def checkReverse(xl: BigInt, q: Int, m: Int): Boolean = {
    val x = constructX(xl, q, m)
    x == rev(lastNdigits(x * x, q))
  }                                               //> checkReverse: (xl: scala.math.BigInt, q: Int, m: Int)Boolean

 for {
    q <- 2 to 18
    m <- List(math.ceil(q*1.0/2).toInt)
    xl <- BigInt(1) until BigInt(10).pow(m)
    if ( checkReverse(xl,q,m) )
  } println (xl,constructX(xl, q, m),constructX(xl,q,m).pow(2))
                                                  //> (63,963,927369)
                                                  //| (67,9867,97357689)
                                                  //| (714,69714,4860041796)
                                                  //| (766,65766,4325166756)
                                                  //| (7056,6317056,39905196507136)
                                                  //| (9553,90899553,8262728735599809)
                                                  //| (5719,169605719,28766099917506961)
                                                  //| (81082,4270981082,18241279402801890724)
                                                  //| (287587,96528287587,9317710304478578282569)
                                                  //| (256742,465454256742,216647665119247652454564)
                                                  //| (612536,692153612536,479076623346635216351296)
                                                  //| (19071841,182921919071841,33460428476925148170919129281)
                                                  //| (69669834,655785969669834,430055238015804438966969587556)
                                                  //| (578750084,650700037578750084,423410538904986771480057875730007056)-
 
}