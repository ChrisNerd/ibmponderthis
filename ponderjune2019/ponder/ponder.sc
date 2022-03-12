import scala.collection.immutable.TreeSet
import java.text.SimpleDateFormat

/*
https://www.wolframalpha.com/input/?i=sqrt(2%5E40)%2Bsqrt(2%5E30)%2Bsqrt(2%5E26)%2Bsqrt(2%5E24)%2Bsqrt(2%5E20)%2Bsqrt(2%5E18)%2Bsqrt(2%5E16)%2Bsqrt(2%5E12)%2Bsqrt(2%5E10)%2Bsqrt(2%5E8)%2Bsqrt(2%5E6)%2Bsqrt(2%5E13*5%5E5*7%5E2)%2Bsqrt(2%5E9*3%5E6*5%5E2*7%5E1)-sqrt(3%5E5*5%5E8*7%5E3)-sqrt(2%5E3*3%5E7*5%5E5*7%5E5)
sqrt(2^40)+sqrt(2^30)+sqrt(2^26)+sqrt(2^24)+sqrt(2^20)+sqrt(2^18)+sqrt(2^16)+sqrt(2^12)+sqrt(2^10)+sqrt(2^8)+sqrt(2^6)+sqrt(2^13*5^5*7^2)+sqrt(2^9*3^6*5^2*7^1)-sqrt(3^5*5^8*7^3)-sqrt(2^3*3^7*5^5*7^5)
=3.5451270962946032996925296969951518242455994099013914... × 10^-16
Needs to be run with TONS of memory
scalac ponder.sc && scala -J-Xms6g -J-Xmx10g ponder
and will eventually output:
(Solution Found!!!,a: 1254400000 List(13, 0, 5, 2) b: 32558203125 List(0, 5, 8, 3) a: 918922725000 List(3, 7, 5, 5) b: 65318400 List(9, 6, 2, 1))
but will not terminate.

*/

object ponder {

 // convert normal string to hex bytes string
 def string2hex(str: String): String = {
  str.toList.map(_.toInt.toHexString).mkString
 }                                                //> string2hex: (str: String)String
 
val names = List("Bob Guernsey", "Ian Brackenbury", "Lewis Terman", "Dave Ehnebuske", "William Tetzlaff",
"Joanne Martin",
"Emily Plachy",
"Rashik Parmar",
"Andrea Martin",
"Susan Schreitmueller Smartt","Turing Award","compilers", "program optimization",  "parallelization", "IBM Fellow"
)                                                 //> names  : List[String] = List(Bob Guernsey, Ian Brackenbury, Lewis Terman, D
                                                  //| ave Ehnebuske, William Tetzlaff, Joanne Martin, Emily Plachy, Rashik Parmar
                                                  //| , Andrea Martin, Susan Schreitmueller Smartt, Turing Award, compilers, prog
                                                  //| ram optimization, parallelization, IBM Fellow)
 val hexString = string2hex("Ò")//Frances E. Allen")
                                                  //> hexString  : String = d2
 // 4672616e63657320452e20416c6c656e
 
 val bigInt = BigInt(hexString, 16);              //> bigInt  : scala.math.BigInt = 210

 def factorsOnlyLessThan10(bn : BigInt): Boolean =
  if (bn <= 1)
   true
  else
   List(2,3,5,7).find( f => bn % f == 0) match {
    case None => false
    case Some(f) => factorsOnlyLessThan10(bn/f)
   }                                              //> factorsOnlyLessThan10: (bn: BigInt)Boolean

 factorsOnlyLessThan10(bigInt)                    //> res0: Boolean = true
 
 names.map(n => (n, factorsOnlyLessThan10(BigInt(string2hex(n),16))))
                                                  //> res1: List[(String, Boolean)] = List((Bob Guernsey,false), (Ian Brackenbury
                                                  //| ,false), (Lewis Terman,false), (Dave Ehnebuske,false), (William Tetzlaff,fa
                                                  //| lse), (Joanne Martin,false), (Emily Plachy,false), (Rashik Parmar,false), (
                                                  //| Andrea Martin,false), (Susan Schreitmueller Smartt,false), (Turing Award,fa
                                                  //| lse), (compilers,false), (program optimization,false), (parallelization,fal
                                                  //| se), (IBM Fellow,false))
// Epoch timestamp: 1172016000
//Timestamp in milliseconds: 1172016000000
//Date and time (GMT): Wednesday, February 21, 2007 12:00:00 AM
//Epoch timestamp: 1172102400
//Timestamp in milliseconds: 1172102400000
//Date and time (GMT): Thursday, February 22, 2007 12:00:00 AM
 BigInt("1172102400") - BigInt("1172016000")      //> res2: scala.math.BigInt = 86400
 BigInt(1172102400) - BigInt(1172016000)          //> res3: scala.math.BigInt = 86400
 
 val ans = (BigInt("1172016000") to BigInt("1172102400")).filter(x=>factorsOnlyLessThan10(x))
                                                  //> ans  : scala.collection.immutable.IndexedSeq[BigInt] = Vector()
   val df:SimpleDateFormat = new SimpleDateFormat("yyyy-MM-dd")
                                                  //> df  : java.text.SimpleDateFormat = java.text.SimpleDateFormat@f67a0200
  

 def generateETuplesThatSumToN(n: Int, depth: Int): IndexedSeq[List[Int]] = depth match {
  case 1 => IndexedSeq(List(n))
  case _ => if (n > 0)
  for {
   i <- 0 to n
   rest <- generateETuplesThatSumToN(n-i, depth - 1)
  } yield i :: rest
  else
   IndexedSeq(List.fill(depth)(0))
 }                                                //> generateETuplesThatSumToN: (n: Int, depth: Int)IndexedSeq[List[Int]]
 
 generateETuplesThatSumToN(2, 4)                  //> res4: IndexedSeq[List[Int]] = Vector(List(0, 0, 0, 2), List(0, 0, 1, 1), Li
                                                  //| st(0, 0, 2, 0), List(0, 1, 0, 1), List(0, 1, 1, 0), List(0, 2, 0, 0), List(
                                                  //| 1, 0, 0, 1), List(1, 0, 1, 0), List(1, 1, 0, 0), List(2, 0, 0, 0))

 def testForSmallness(x: scala.math.BigDecimal) = {
  x.abs < BigDecimal(10).pow(-15) && x.abs > BigDecimal(10).pow(-28) }
                                                  //> testForSmallness: (x: scala.math.BigDecimal)Boolean

 val sqrt2 = BigDecimal("1.414213562373095048801688724209698078569671875376948073176")
                                                  //> sqrt2  : scala.math.BigDecimal = 1.4142135623730950488016887242096980785696
                                                  //| 71875376948073176
 val sqrt3 = BigDecimal("1.732050807568877293527446341505872366942805253810380628055")
                                                  //> sqrt3  : scala.math.BigDecimal = 1.7320508075688772935274463415058723669428
                                                  //| 05253810380628055
 val sqrt5 = BigDecimal("2.236067977499789696409173668731276235440618359611525724270")
                                                  //> sqrt5  : scala.math.BigDecimal = 2.2360679774997896964091736687312762354406
                                                  //| 18359611525724270
 val sqrt7 = BigDecimal("2.645751311064590590501615753639260425710259183082450180368")
                                                  //> sqrt7  : scala.math.BigDecimal = 2.6457513110645905905016157536392604257102
                                                  //| 59183082450180368
 
 def calcSqrtFromTuple( exponentsTuple: List[Int]) : BigDecimal = {
  // Could have been a bit clever and tried
  // if (exponentsTuple(0) % 2 == 0)
  //  BigInt(2).pow(exponentsTuple(0)/2)
  // else
  //  sqrt2.pow(exponentsTuple(0))
  // *
  // if ...
  val sqr =
   sqrt2.pow(exponentsTuple(0)) *
   sqrt3.pow(exponentsTuple(1)) *
   sqrt5.pow(exponentsTuple(2)) *
   sqrt7.pow(exponentsTuple(3))
  sqr - BigDecimal(sqr.toBigInt())
 }                                                //> calcSqrtFromTuple: (exponentsTuple: List[Int])BigDecimal
 def calcNFromTuple( exponentsTuple: List[Int]) : BigInt = {
   BigInt(2).pow(exponentsTuple(0)) *
   BigInt(3).pow(exponentsTuple(1)) *
   BigInt(5).pow(exponentsTuple(2)) *
   BigInt(7).pow(exponentsTuple(3))
 }                                                //> calcNFromTuple: (exponentsTuple: List[Int])BigInt
 
calcSqrtFromTuple(List(2,2,3,4))                  //> res5: BigDecimal = 0.019926924690853721485293034976066097708988628942814666
                                                  //| 
calcNFromTuple(List(2,2,3,4))                     //> res6: BigInt = 10804500
case class smoothNum(fractionalSquareRoot: BigDecimal, exponentsTuple: List[Int], n: BigInt) extends Ordered[smoothNum] {
  def compare(that: smoothNum) = {
   val d = this.fractionalSquareRoot - that.fractionalSquareRoot
   if (testForSmallness(d))
     println("Solution Found!!!", this + " " + that)
	 this.fractionalSquareRoot compare that.fractionalSquareRoot
	 }
	 override def toString() = this.n.toString() + " " + this.exponentsTuple
 }

 case class smoothNumDiff(diff: BigDecimal, s1: smoothNum, s2: smoothNum) extends Ordered[smoothNumDiff] {
	def compare(that: smoothNumDiff) = {
   val d = this.diff - that.diff
   if (testForSmallness(d))
     println("Solution Found!!!", this + " " + that)
	 if (d < 0)
	  -1
	 else if (d > 0)
	  1
	 else 0
	 }
	override def toString() = "a: " + this.s1.toString() + " b: " + this.s2.toString()
 }
 

def main(args: Array[String]) = {
 println("Starting")
 class Hamming extends Iterator[BigInt] {
  import scala.collection.mutable.Queue
  val q2 = new Queue[BigInt]
  val q3 = new Queue[BigInt]
  val q5 = new Queue[BigInt]
  val q7 = new Queue[BigInt]
  
  def enqueue(n: BigInt) = {
    q2 enqueue n * 2
    q3 enqueue n * 3
    q5 enqueue n * 5
    q7 enqueue n * 7
  }
  def next = {
    val n = q2.head min q3.head min q5.head min q7.head
    if (q2.head == n) q2.dequeue
    if (q3.head == n) q3.dequeue
    if (q5.head == n) q5.dequeue
    if (q7.head == n) q7.dequeue
    enqueue(n)
    n
  }
  def hasNext = true
  List(q2, q3, q5, q7) foreach (_ enqueue 1)
 }
 new Hamming takeWhile { _ < BigInt("1577840400") }  foreach(x => println(df.format(x * 1000L) + " " + x.toString()))
 
 
 
 
 println("Starting3")
 val ans2 = (BigInt("0") to BigInt("1560674304")).filter(x=>factorsOnlyLessThan10(x))
 .foreach(x => println(df.format(x * 1000L)))
 println("Starting2")
 
 def addNumbersToTreeWithExponentSumTot( treeOfPairs: TreeSet[smoothNumDiff], oldTree: TreeSet[smoothNum], tot: Int): Unit = {
  val newNumbers = for {
   eTuple <- generateETuplesThatSumToN(tot, 4)
   if ( !eTuple.forall(_ % 2 == 0) )
  }yield smoothNum(calcSqrtFromTuple(eTuple), eTuple, calcNFromTuple(eTuple))

  addNumbersToTreeWithExponentSumTot(
   treeOfPairs ++ ( for {
    n <- newNumbers
    o <- oldTree
   } yield smoothNumDiff((n.fractionalSquareRoot - o.fractionalSquareRoot).abs, n,o))
   , oldTree ++ newNumbers, tot +1)
 }

 addNumbersToTreeWithExponentSumTot(TreeSet[smoothNumDiff](), TreeSet[smoothNum](), 1)

 println("Should not get here")

 TreeSet[smoothNum]() ++ (for {
  total <- Iterator.from(1)
  eTuple <- generateETuplesThatSumToN(total, 4)
  if ( !eTuple.forall(_ % 2 == 0) )
 }
 yield smoothNum(calcSqrtFromTuple(eTuple), eTuple, calcNFromTuple(eTuple)) )
}                                                 //> main: (args: Array[String])Unit

 
 // C = 2 to 30 yielded no solutions!!! (with B <- C --A (not the extra -- ...)
/* val C = (2 to 15).filter(x=> math.pow(math.sqrt(x).round,2)!= x).filter(factorsOnlyLessThan10).toSet
 C.size
 def binomialCoefficient(n:Int, k:Int)=fact(n) / (fact(k) * fact(n-k))
 def fact(n:Int): scala.math.BigInt = if (n==0) scala.math.BigInt(1) else fact(n-1) * n
 val powers = for( i <- 2 to C.size) yield (binomialCoefficient(C.size, i) * math.pow(2,(C.size-i)).toLong)
 powers.sum
 */
 // So say we have .1, .2, .6, .8 in t (where t is a treeset of fractionalPart(sqrt(n)) values)
 // That means we have .1, .4, .2 in d. (where d is a treemap of BigDecimal -> (Int, Int) )
 // Then say, we add .3 to t.
 // Then we must extract .2, and .6 from t somehow (maxBefore and minAfter would be nice...) (we can probably do this with slice( indexOf(3) -1, indexOf(3) +1)
 // We then add (.3 -.2 = .1, and .6-.3=.3) to d.
 // We notice that .1 is already suuuuper close (but too close) to the .1 already in d ( 10^-30 < q < 10^-15), and return that as the solution.
 //t.maxBefore(3)
// t.iteratorFrom(3)
 //maxBefore(3)
 
/* val doublePowerSet = ( for {
  A <- C.subsets().filter(_.size > 1)
	B <- (C -- A -- (2 to 30).toSet).subsets()
  if {
   val bnDouble = calcSqrtBn(A, B).abs
   val bn = bnDouble.pow(2).round(mc).toInt
   val absDiff = (bnDouble - sqrtCache.getOrElseUpdate(bn.toInt, sqrt(new java.math.BigDecimal(bn),128))).abs
   ! A.contains(bn) && ! B.contains(bn) && absDiff < math.pow(10,-15) && absDiff != 0 && absDiff > math.pow(10,-28) &&
   factorsOnlyLessThan10(bn)
  }
 } yield (A,B,calcSqrtBn(A,B))//, (calBn(A, B) - calcBn(A, B).round(mc)).abs)
 ).foreach(println)
 */
 //doublePowerSet.toList.sortBy( _._4)
}