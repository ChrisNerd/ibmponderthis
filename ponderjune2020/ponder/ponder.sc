object ponder {
  import scala.io.Source
  val MBigInt = BigInt("3031634148236289733373855928919180891127808")
  val ABigInt = BigInt("12142680281284711468101282998309016699980172")
  
  def factor(n : BigInt, startAt: BigInt = 2) : Map[BigInt, Int] = {
   Iterator.iterate(startAt)(_ + 1).takeWhile( x => x*x <= n ).find( x => n.mod(x) == 0) match {
    case Some(x) => Map(x -> 1) ++ factor(n/x, x).map{ case (k,v) => k -> (v + Map(x -> 1).getOrElse(k,0)) }
    case None => Map(n -> 1)
   }
  }
  
  factor(3)
  val M = factor(MBigInt)
  factor(36)
  println(M)
 
 
  // https://www.math.upenn.edu/~deturck/m170/wk3/lecture/sumdiv.html
  // Sum of divisors (including itself)
  def sigma( m: Map[BigInt, Int] ): BigInt =
  {
   m.map{ case (p,k) => (p.pow(k+1) - 1 ) / (p-1)}.product
  }
 
  def allFactors( m: Map[BigInt, Int] ): Set[ Map[BigInt, Int]] = m.headOption match {
   case Some((p, e)) => (for {
    ee <- 0 to e
    fac <- allFactors(m - p)
   } yield if (fac == Map(BigInt(1) -> 1)) Map(p->ee) else Map(p-> ee) ++ fac ).toSet
   
   case _ => Set( Map(BigInt(1) -> 1 ))
  }
 
 allFactors(Map( BigInt(2)->2, BigInt(3)->1))
 sigma(Map( BigInt(2)->2, BigInt(3)->1))
 
 def mapToBigInt(m: Map[BigInt, Int]): BigInt =
  m.map{ case (p,k) => p.pow(k) }.product
 
 allFactors(Map( BigInt(2)->2, BigInt(3)->1)).map(mapToBigInt)
 allFactors(Map( BigInt(2)->2, BigInt(3)->1)).map(mapToBigInt).sum
 
                
allFactors( factor(BigInt(36)) ).map(mapToBigInt(_) + 1).filter(_.isProbablePrime(100)).toList.sorted.reverse
                                                  
 def invPhi(m: BigInt /*Map[BigInt, Int]*/ ) : Set[ BigInt /*Map[BigInt, Int] */] = {
 // Letting p run through all (p-1)| m, and d through all phi(p^d) | m, all solutions can be obtained.
 // To get the primes p for which (p-1)|m, we write out all the divisors of m; add 1 to each of them and retain the primes alone.
 if ( m >= 3  && m%2 != 0) Set()  // There is no x for which phi(x) is odd (other than 1 of course).
  else
 if (m <= BigInt(2)) Set(BigInt(1))
 else
 
 
 (for{
 
 // probably don't want to convert p to a bigInt yet...
 
  p <- allFactors( factor(m) ).map(mapToBigInt(_) + 1).filter(_.isProbablePrime(100)).toList.sorted.reverse
  d <- Stream.from(1).takeWhile{ dd =>   {
  //println("m " + m + " p " + p + " d " + dd)
  }
  m.mod( p.pow(dd-1) * (p-1)) == 0 } // to factor(m)(p-1)
  

  // p=37, d=1, phi(37^1) = 36, m/36 = 1.
 
 // phi(p^d) = p^(d-1) * (p-1)  // Special cases are when d==1, and when p==2
 // We want inv <- invPhi( m/phi(p^d) )
 // So that's m / (p^(d-1) * (p-1))

 this is an infinite loop when p==2 and d==1.
 check the paper, this is a second step

 inv <- invPhi( m / (p-1) / p.pow(d-1) )

/* inv <- invPhi( if (p==2) // no need to divide by p-1
         m.updated(p, m(p)-d)
   else
       m.updated(p-1, m(p-1) - 1).updated(p, m(p)-d))*/
 }
 yield //Map(p->d).map{ case (k,v) => k -> (v + inv.getOrElse(k,0)) }
 p.pow(d) * inv
 ).toSet
 }
 val p36 = invPhi(36)
 
 
 //invPhi(Map(BigInt(2)->2, BigInt(3)->2))
 //println("p36 " + p36)
                                                  
  
/*	val filename = "/home/shannon/outside/ponder/ponderjune2020/nCandidates.txt"
	/*for (line <- Source.fromFile(filename).getLines) {
    println(line)
    }*/
	

  val fileContents = Source.fromFile(filename).getLines.mkString
 
   val arrayOfStrings = fileContents.split(' ')
 val arrayOfBigInts = arrayOfStrings.map(BigInt(_))
       import scala.annotation.tailrec
      def factorize(x: BigInt): List[BigInt] = {
  @tailrec
  def foo(x: BigInt, a: BigInt = 2, list: List[BigInt] = Nil): List[BigInt] = a * a > x match {
    case false if x % a == 0 => foo(x / a, a, a :: list)
    case false => foo(x, a + 1, list)
    case true => x :: list
  }
 
  foo(x)
}
 
def properDivisors(n: BigInt): List[BigInt] = {
  val factors = factorize(n)
  val products = (1 until factors.length).flatMap(i => factors.combinations(i).map(_.product).toList).toList
  (BigInt(1) :: products).filter(_ < n)
}

//val aliquotSum = arrayOfBigInts.map(n => properDivisors(n).sum)
//aliquotSum.filter( _ == BigInt("12142680281284711468101282998309016699980172"))
// println("Solution Found n == " + n)
  
 import sys.process._
 
 "ls -al" !
 
 "echo 'test' | ls"    !
 
 "echo 'sigma(12)' | /home/shannon/outside/ponder/ponderjune2020/krm_calc/calc " !
 val numProcs = ("ps auxw" #| "wc -l").!!.trim
 
 val sig12 = ("echo 'sigma(12)'" #| "/home/shannon/outside/ponder/ponderjune2020/krm_calc/calc").!!.trim
 import java.io.ByteArrayOutputStream
 import java.io.PrintWriter
 def runCommand(cmd: Seq[String]): (Int, String, String) = {
  val stdoutStream = new ByteArrayOutputStream
  val stderrStream = new ByteArrayOutputStream
  val stdoutWriter = new PrintWriter(stdoutStream)
  val stderrWriter = new PrintWriter(stderrStream)
  val exitValue = cmd.!(ProcessLogger(stdoutWriter.println, stderrWriter.println))
  stdoutWriter.close()
  stderrWriter.close()
  (exitValue, stdoutStream.toString, stderrStream.toString)
}



 case class ProcessInfo(stdout: String, stderr: String, exitCode: Int)

 object CommandRunner {

  def runCommandAndGetOutput(command: Seq[String]): ProcessInfo = {
    val stdout = new StringBuffer
    val stderr = new StringBuffer
    println("in run command")
    
    val p = command run ProcessLogger( stdout append _, stderr append _)
    
//    val lines = command lineStream_! ProcessLogger(stdout append _, stderr append _)
    println(stdout)
    println("in2")
    println(stderr)
    println("in3")
    ProcessInfo(stdout.toString(), stderr.toString(), p.exitValue())
  }
 }
  
 
  /** Run a command, collecting the stdout, stderr and exit status */
 def run(in: Seq[String]): (List[String], List[String], String) = {
  val qb = Process(in)
  var out = List[String]()
  var err = List[String]()
 println("in run begin")
 
 
  val exit = qb !! ProcessLogger((s) => out ::= s, (s) => err ::= s)
 println("in run end")
  (out.reverse, err.reverse, exit)
 }
  
  
                                                  
 def aliquotSum(n: BigInt) =
 {
 val inString = Seq("/home/shannon/outside/ponder/ponderjune2020/krm_calc/calc", "sigma(" + n.toString +")")
 println("in AliquotSum")
 println(inString)
 val outs = CommandRunner.runCommandAndGetOutput(inString)
 
 println(" exit code " + outs.exitCode)
 if ( inString.!  == 0 )
{
  val out = inString.!!
  println("here1")
  println(out)
  println("here2")
  
  val splitout = out.split('\n')
//  println("here3")
  
 // println(splitout)
  /*val outBigInt =*/ BigInt(splitout.last) - n
  }
  else
  BigInt("1")
  }
  
  
 aliquotSum(12)
 
 */
 def main(args: Array[String]) = {

println(M)

/*
  val aliquotSumResults = arrayOfBigInts.map{ p =>
 // println(p)
  val a = aliquotSum(p)
 // println("A sum " + a)
  (p,a)
   }
 
 println("Done going through file. Searching for solution")
  
  //aliquotSumResults.head
  println("size " + aliquotSumResults.size)
  
  val solutions = aliquotSumResults.filter( _._2 == BigInt("12142680281284711468101282998309016699980172"))
  println("Sol size " + solutions.size)
  
  solutions.foreach{ n =>
 println("Solution Found n == " + n._1) }
*/
 
 }
   
  
}