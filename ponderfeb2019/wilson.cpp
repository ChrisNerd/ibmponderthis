#include <iostream>
#include <math.h>
#include <vector>
#include <map>
#include <unordered_map> 
//#include <stdlib.h>
#include <signal.h>
/*
g++ wilson.cpp && time echo "1 200000" | ./a.out 
Lower &, Upper numbers. Lower 1 Upper 200000
Prime size200001
1093486725 59978
T=1093486724



CTRL-C detected
Solutions
1165703083461682665 1958318009
1167912712342765305 1960173159
1171776304617085545 1963412723
1177817661760683945 1968467625
1177932616004759235 1968563683
1192879591764083655 1981014022
1211279064269490615 1996233552
1211314794035437275 1996262993
1213811644318082985 1998319356
1218140798422573005 2001879765
End SigInt Handler



Solution found S=1165703083461682665 n = 1958318009
1 0 2 13 6 8 12 7 3 11 4 15 10 5 14 9 
END SOLUTION

Solution found S=1167912712342765305 n = 1960173159
1 0 3 5 4 2 6 12 8 11 13 10 7 14 15 9 
END SOLUTION

Solution found S=1171776304617085545 n = 1963412723
1 0 4 2 15 12 5 7 3 8 14 11 13 10 6 9 
END SOLUTION
Solution found S=1177817661760683945 n = 1968467625
1 0 5 8 7 2 14 12 4 11 6 13 3 15 10 9 
END SOLUTION

Solution found S=1177932616004759235 n = 1968563683
1 0 5 8 13 11 7 9 2 10 4 15 6 14 12 3 
END SOLUTION

Solution found S=1192879591764083655 n = 1981014022
1 0 8 13 15 5 10 11 2 6 14 9 4 3 12 7 
END SOLUTION

Solution found S=1211279064269490615 n = 1996233552
1 0 12 15 5 3 14 4 2 13 10 6 8 9 11 7 
END SOLUTION

Solution found S=1211314794035437275 n = 1996262993
1 0 12 15 7 4 6 3 2 9 10 8 5 14 13 11 
END SOLUTION

Solution found S=1213811644318082985 n = 1998319356
1 0 13 8 5 3 4 2 6 11 12 15 14 7 10 9 
END SOLUTION
S 1213833504342
Solution found S=1218140798422573005 n = 2001879765
1 0 14 7 11 4 9 10 2 8 6 3 5 15 12 13 
END SOLUTION


(Solution found!!!,2385834551)
1730223302862534330

real    0m0.079s
user    0m0.077s
sys     0m0.001s
  */

//https://oeis.org/A002088/a002088.pdf
//https://en.wikipedia.org/wiki/Prime-counting_function
/*
  10 ! Sum Euler's TOTIENT or Phi function (n) from L to U
  20 T=0 @ INPUT 'Low &, Up Nbr. ';L,U
  30 FOR I=L TO U
  40 K=1 @ S=1 @ N=I
  50 R=PRIM(N) @ IF R=1 THEN 110  // There's no GOTO here
  60 P=N/R
  70 Q=PRIM(P)
  80 IF R=Q THEN K=K+1 @ P=P/R @ GOTO 70
  90 S=S*(R-1)*R^(K-1)
  100 N=P @ R=Q @ K=1 @ GOTO 50
  110 T=T+S @ NEXT I @ DISP T @ STOP
*/

/*
OK, I think I got it.
Write n in its prime decomposition state.
n=p1^k1 * p2^k2 * p3^k3...
There's an identity
phi(n^k) = n^(k-1)*phi(n)
and if n is prime
phi(n) = n-1

Also phi(ab) = phi(a)*phi(b) when a and b are coprime.


So if n=p1^k1 * p2^k2 * p3^k3...
let a= p1^k1 and b=p2^k2 * p3^k3..., (a and b are coprime)
phi(n) = phi(ab) = phi(a)*phi(b)
= phi(p1^k1) * phi(p2^k2 * p3^k3...)
and since p1 is prime, from the first identity
= p^(k-1) *phi(pi) * phi(p2^k2 * p3^k3...)
= p^(k-1) *(n-1)   * phi(p2^k2 * p3^k3...)
Then we can iterate for p2, p3,...
That's where
90 S=S*(R-1)*R^(K-1) comes in. Could also be written as S=S* (R^K-R^(K-1))
So we know that R corresponds to p1, p2,p3 etc.
We're doing R=PRIM(P), 

Somehow we have to know what p1, p2, p3 are. Is that encoded in PRIM somehow?
Maybe PRIM(n) is the smallest prime that divides n!!!
We know that can do do N/R right away. And there's probably nothing fancy about about PRIM, just an array.
Then we get to line 100 N=P, we avoid an infinite loop because P has been decreased!!!
Yes!!!!!

Whoa, what if we set PRIM(n) to the *largest* prime that divides n?
Yes, that works too.

I think those are the only two choices for PRIM though. You can't take the second smallest, or the middle etc.

God I hate imperitive style, this could be written so much clear in functional.

val L = 1                                         //> L  : Int = 1
val U = 20                                        //> U  : Int = 20

def smallestPrimeThatDividesN(n: Int): Int = {
 (2 to math.sqrt(n).toInt).find(i => n%i == 0) match
  { 
   case Some(i) => i
   case _ => n 
  }
 }                                                //> smallestPrimeThatDividesN: (n: Int)Int

val listOfSmallestPrimesThatDividesN = (L to U).map(n => (n, smallestPrimeThatDividesN(n))).toMap
                                                  //> listOfSmallestPrimesThatDividesN  : scala.collection.immutable.Map[Int,Int] 
                                                  //| = Map(5 -> 5, 10 -> 2, 14 -> 2, 20 -> 2, 1 -> 1, 6 -> 2, 9 -> 3, 13 -> 13, 2
                                                  //|  -> 2, 17 -> 17, 12 -> 2, 7 -> 7, 3 -> 3, 18 -> 2, 16 -> 2, 11 -> 11, 8 -> 2
                                                  //| , 19 -> 19, 4 -> 2, 15 -> 3)
// Must start this array at n=0 so the indexing matches.
val listOfSmallestPrimesThatDividesNArray = (0 to U).map(n => smallestPrimeThatDividesN(n)).toArray
                                                  //> listOfSmallestPrimesThatDividesNArray  : Array[Int] = Array(0, 1, 2, 3, 2, 5
                                                  //| , 2, 7, 2, 3, 2, 11, 2, 13, 2, 3, 2, 17, 2, 19, 2)

def listOfPrimeFactorsOfN(n: Int): List[Int] = listOfSmallestPrimesThatDividesNArray(n) match
{
 case p if (p != 1) =>  p :: listOfPrimeFactorsOfN(n/p)
 case _  => Nil
}                                                 //> listOfPrimeFactorsOfN: (n: Int)List[Int]

val eulerPhi = (L to U).map(n => listOfPrimeFactorsOfN(n).groupBy(identity).mapValues(_.size).map{ case(p,k) => (p-1)*math.pow(p,k-1)}.product.toInt)
                                                  //> eulerPhi  : scala.collection.immutable.IndexedSeq[Int] = Vector(1, 1, 2, 2, 
                                                  //| 4, 2, 6, 4, 6, 4, 10, 4, 12, 6, 8, 8, 16, 6, 18, 8)

val T = eulerPhi.scanLeft(0)( _ + _ )             //> T  : scala.collection.immutable.IndexedSeq[Int] = Vector(0, 1, 2, 4, 6, 10, 
                                                  //| 12, 18, 22, 28, 32, 42, 46, 58, 64, 72, 80, 96, 102, 120, 128)


*/
using namespace std;


// man compare to scala!
// a.toString.distinct.size == 10
bool distinct(unsigned long long int a)
{
  if (a < pow(16,15) || a >= pow(16,16))
    return false;
  int ele[16]={0};
  while(a)
    {
        int t=a%16;
        if (ele[t]==1)
	  return false;
	ele[t]=1;
        a=a/16;
    }
  return true;
}

std::unordered_map<unsigned long long int,unsigned long long int> SumEulerphiFromMathProblems123Map;

//val SumEulerphiFromMathProblems123Map = collection.mutable.Map[BigInt, BigInt]()
  // From https://mathproblems123.wordpress.com/2018/05/10/sum-of-the-euler-totient-function/
  // See also https://trizenx.blogspot.com/2018/11/partial-sums-of-arithmetical-functions.html

unsigned long long int SumEulerphiFromMathProblems123(unsigned long long int n);

unsigned long long int SumEulerphiFromMathProblems123Function( unsigned long long int n)
{
  unsigned long long int s1 = 0;
  for(long m=2; m <= sqrt(n); m++)
    {
      s1+= SumEulerphiFromMathProblems123( n/m);
    }
  
  unsigned long long int s2 = 0;
  for(long d=1; d <= sqrt(n); d++)
    {
      if (d==n/d)
	break;
      s2+= (n/d - n/(d+1)) * SumEulerphiFromMathProblems123(d);
    }
  

  return  n * (n+1)/2 - s1 - s2;
}
/*
    (n) * (n + 1) / 2 -
      (2.toLong to sqt(n).toLong).map(m => SumEulerphiFromMathProblems123(n / m)).sum -
      (1.toLong to sqt(n).toLong).takeWhile(d => d != n / d).map(d => (n / d - n / (d + 1)) * SumEulerphiFromMathProblems123(d)).sum
*/

unsigned long long int SumEulerphiFromMathProblems123(unsigned long long int n)
{
  unsigned long long int r = SumEulerphiFromMathProblems123Map[n];
  if (r == 0)
    {
      r = SumEulerphiFromMathProblems123Function(n);
      SumEulerphiFromMathProblems123Map[n] = r;
    }
  return r;
}

/*  def SumEulerphiFromMathProblems123(n: BigInt) =
    {
      //      println("Called sumeupler with ", n)
      //     println("Size of Map ", SumEulerphiFromMathProblems123Map.size)
      SumEulerphiFromMathProblems123Map.getOrElseUpdate(n, SumEulerphiFromMathProblems123Function(n))
    }
*/
unsigned long long int ipow( unsigned long long int base, unsigned long long int exp )
{
    unsigned long long int result = 1ULL;
    while( exp )
    {
        if ( exp & 1 )
        {
            result *= (unsigned long long int)base;
        }
        exp >>= 1;
        base *= base;
    }
    return result;
}


unsigned long long int generateNextN(unsigned long long int n)
{
  unsigned long long int Sn  = SumEulerphiFromMathProblems123(n)+1;
  unsigned long long int Sn1 = SumEulerphiFromMathProblems123(n+1)+1;
  /*
  cout << "Sn = " << Sn << endl;
  cout << "n = " << n  << endl;
  cout << "n+1 = " << n+1 << endl;
  cout << "Sn1 = " << Sn1 << endl ;
  */
  int SnVec[16]={0};
  int Sn1Vec[16]={0};
  for(int i =0; i< 16; i++)
    {
      SnVec[15-i]=Sn%16;
      Sn/=16;
      Sn1Vec[15-i]=Sn1%16;
      Sn1/=16;
    }
  /*
    cout << "SnVec" << endl;
  for (int i = 0; i < 16; i++)
    cout << SnVec[i] << " " ;
  cout << endl;
  cout << "Sn1Vec" << endl;
  for (int i = 0; i < 16; i++)
    cout << Sn1Vec[i] << " " ;
  cout << endl;
*/
  
  vector<int> longestPrefix;
  bool allDistinct[16] = {false};
  // OK, here we're searching for the first instance of a duplicate
  // as well as comparing the two lists element by element.
  // Both at once.
  // Allowing the first occurance of a duplicant.
  bool allowFirst = true;
  for(int i=0;i<16; i++)
    {
      if (SnVec[i] == Sn1Vec[i] && (!allDistinct[SnVec[i]] || allowFirst))
	{
	  if (allDistinct[SnVec[i]])
	    allowFirst = false;
	  allDistinct[SnVec[i]] = true;
	  longestPrefix.push_back(SnVec[i]);
	}
      else
	break;
    }
  /*
  cout << "Longest Prefix" << endl;  
  for (int i = 0; i < longestPrefix.size(); i++)
    cout << longestPrefix[i] << " " ;
  cout << endl;
  */

  if (longestPrefix.size() == 0)
    return n+1;
  // Else construct the next lowest

  unsigned long long int Lnum = 0;
  for (int i=0; i< longestPrefix.size(); i++)
    Lnum += longestPrefix[i] * pow(16, longestPrefix.size() -1 - i);
  
  while(1)
    {
      //      cout << "Lnum " << Lnum << endl;
      Lnum++;
      //      compare longestPrefix to LnumVec
      //      if all different
      //	       return lum;
      //	else
      //	  lnum++
      unsigned long long int t = Lnum;

      bool ele[16]={false};
      bool allD = true;
      while(t)
	{
	  int a=t%16;
	  if (ele[a])
	    allD = false;
	  ele[a]=true;
	  t=t/16;
	}
      if (allD)
	break;
    }
  vector<int> LnumVec(longestPrefix.size());       // might have to use a new or something... remember to delete
  //  cout << "Here1 Lnum= " << Lnum << endl;
  
  unsigned long long int t = Lnum;
  for(int i = 0; i < longestPrefix.size(); i++)
    {
      LnumVec[longestPrefix.size() - 1-i]= t%16;
      t/=16;
    }
  
  /*
  for(int i=0; i< LnumVec.size(); i++)
	cout << LnumVec[i] << " " ;
      cout << endl;
  cout << "Next lowest Lnum " << Lnum << endl;
  */
  // Now to fill in the rest of the Ls with the lowest distinct values
  // so L will be a the next smallest permutation of all 16 digits

  int LFinal[16];
  bool covered[16] = {false};
  int i;
  for (i =0; i < LnumVec.size() ; i++)
    {
      covered[LnumVec[i]] = true;
      LFinal[i] = LnumVec[i];
    }

  for(i=i; i < 16; i++)
    {
      int j=0;
      while (covered[j])
	j++;
      LFinal[i]=j;
      covered[j]=true;
    }

  unsigned long long int LnumFinal = 0;
  for (int i=0; i< 16; i++)
    LnumFinal += LFinal[i] * pow(16, 15 - i);
  /*
  cout << "LFinal " << endl;
  for (int i=0; i < 16; i++)
    cout << LFinal[i] << " " ;
  cout << "LnumFinal " << LnumFinal << endl;
*/

  return sqrt(LnumFinal) * M_PI / sqrt(3.0);
}


/*val sumeuler = Stream.iterate(startingValue){ last =>
					      
       val lastS = toHexList(SumEulerphiFromMathProblems123(last) + 1).reverse
  		 val bound = Stream.iterate(1)(_ * 2)
  		 val nextS = bound.map(b=> (b,toHexList(SumEulerphiFromMathProblems123(last+b) + 1).reverse))
  		 val commonPrefix = nextS.map{ case (b,ns) => (b,ns, ns.zip(lastS).takeWhile(x=> x._1 == x._2)   )}
       // Find the first one that has no duplicates in the common prefix
       val lastCommonPrefix = commonPrefix.takeWhile(x => x._3.size > 0 && x._3.size != x._3.distinct.size).lastOption
       //println("h2", last, lastS, nextS, commonPrefix)
       lastCommonPrefix match {
        case Some(x) => {
         //println("found ", last, x._1)
         last+ x._1
        }
        case None => {
        // println("didn't find ", last)
         last + 1
        }
       }
	    }.map(x => (SumEulerphiFromMathProblems123(x) + 1, x)).zipWithIndex
*/
vector<unsigned long long int> solS;
vector<unsigned long long int> soln;
volatile bool STOP = false;
void sigint_handler(int sig) {
    printf("\nCTRL-C detected\n");
    cout << "Solutions" << endl;
    for(int i = 0; i< solS.size(); i++)
      cout << solS[i] << " " << soln[i] << endl;
    cout << "End SigInt Handler" << endl;
    STOP = true;
}

int main()
{
  signal(SIGINT, sigint_handler);
 
  //unsigned long long int n = sqt(ipow(16,15) * M_PI * M_PI / 3.0); 
  unsigned long long int n = pow(16,7.5) * M_PI /sqrt(3.0);
  cout << "Starting n " << n << endl;
  unsigned long long int count = 0;
  while (true)
    {
      if (STOP) {
	return 0;
      }
      unsigned long long int S = SumEulerphiFromMathProblems123(n) + 1;
      if (count%1000  == 0)
	{
	  cout << "S " << S << endl;
	  cout << "n " << n << endl;
	  int SVec[16];
	  unsigned long long int t = S;
	  for(int i = 0; i < 16; i++)
	    {
	      SVec[15-i]= t%16;
	      t/=16;
	    }
	  for(int i=0; i< 16 ;i++)
	    cout << SVec[i] << " ";
	  cout << endl;
	  
	}
      count++;
      if (distinct(S))
	{
	  cout << "Solution found S=" << S << " n = "  << n << endl;
	  int SVec[16];
	  unsigned long long int t = S;
	  for(int i = 0; i < 16; i++)
	    {
	      SVec[15-i]= t%16;
	      t/=16;
	    }
	  for(int i=0; i< 16 ;i++)
	    cout << SVec[i] << " ";
	  cout << endl;
	  cout << "END SOLUTION"<<endl;
	  solS.push_back(S);
	  soln.push_back(n);
	}
      n = generateNextN(n);
    }
  cout << "Solutions" << endl;
  for(int i = 0; i< solS.size(); i++)
    cout << solS[i] << " " << soln[i] << endl;
  
  long T=0; // Total
  cout << "Lower &, Upper numbers. ";
  int L; // Lower bound of sum (should be 1)
  unsigned long U; // Upper bound of sum (should be 'n' of Big Phi(n))
  cin >> L >> U;
  cout << "Lower " << L << " Upper " << U << endl;

  unsigned long* PRIM = new unsigned long [U+1];
  // PRIM(N) is the lowest prime that divides N.
  // PRIM(0) is garbage.
  // PRIM(1) must be 1.
  PRIM[1] = 1;
  for (unsigned long i=2; i<=U; i++)
    {
      bool isPrime = true;
      //int smallestDivisor = 0;
      //      int n = i;
      for (unsigned long j =2; j <= i; j++)
	{
	  //	  while (n%j == 0)
	  if (i%j == 0)
	    {
	      PRIM[i] = j;
	      isPrime = false;
	      break;
	      //smallestDivisor = j;
	      //	      n/=j;
	    }
	}
      if (isPrime)
	PRIM[i]=i;
      //      else
      //PRIM[i]=largestDivisor;
    }
  cout << "Prime size" << U+1<< endl;
  return 0;
  /*  for (int i=0; i <=U ; i++)
    cout << PRIM[i] << endl;
  */
  
  for (int I=L ; /*T < pow(10,10)*/I <= U; I++){  // 30 FOR I=L TO U
    int K=1; int S=1; int N=I;    //  40 K=1 @ S=1 @ N=I
    while(true){
      int Q;
      int R=PRIM[N];  //50 R=PRIM(N) @ IF R=1 THEN 110
      //      cout << "R=" << R << endl;
      if (R==1)
	break;
      int P=N/R;      // Looks like R is a prime //60 P=N/R
      //      for (int P=N/R; R!=Q; Q=PRIM[P], K++, P/=R){}
      //cout << "here2 p="<< P << " n=" << N << " r=" << R << endl;

      while (true){
	Q=PRIM[P]; // Huh. We're taking the pth prime? Or are we returning 1 if p is prime or something? 0 if its composite? We're only using Q at if (R==Q) //70 Q=PRIM(P)
	// K, it looks like we're repeatedly dividing by R (which is the Nth prime) until
	// R=Q.
	//***
	// So We start with R. And we're essentially calculating k that solves r^k=n
	// But we're working with int divisions.
	// so k=number of times we have to do p=p/r in int divisions.
	// before we get to ... r=q, or nth prime != pth prime???.
	// We're never changing R in this inner loop, so if R==Q once, then we
	// change Q, R!=Q a second time. So K will only be 1 or 2.
	// That sort of makes sense. Phi (n) = n-1 if n is prime!
	// And Phi(mn)=Phi(m)*phi(n) otherwise.
	// So K being 1 or 2 is n being prime or composite.
	// That explains S *= (R-1)*R^(K-1). which is 1*= (R-1) for primes, which 1, 2, 4, 6, 10, 12,16,18,22.
	// and *= (R-1)*R = R^2-R for composites. Hmmm(R-1)*R? For R=2,3,5,7 that's 2, 6, 20, 42
	// S must be form by only those factors. So S=18 is impossible. Is 18 a totient number???!
	/// ***
	//	cout << "here 3 q="<< Q << " k=" <<K << " p=" << P << endl;
	if (R==Q) {
	  //	  cout << "here 3a"<< endl;
	  K++;	//80 IF R=Q THEN K=K+1 @ P=P/R @ GOTO 70
	  P /= R;
	  // continue;
	}
	else
	  break;
      }// Looks like S is phi(i), since we're doing T+=S  
      //      cout << "here 4 r=" << R << " s=" << S << " k= " << K <<endl;
      S *= (R-1)*pow(R,K-1);// 90 S=S*(R-1)*R^(K-1)
      //      cout << "now s=" << S << endl;
      // Really?? Is phi of the form product of (prime-1)*prime^k
      //1 1 2 2 4 2 6 4 6 4 10 4 12 6 8 8
      // (2-1)*2^0=1 (2-1)*2^1=2  (2-1)*2^2=4
      // (3-1)*3^0=2 (3-1)*3^1=6  (3-1)*3^2=18
      // (4-1)*4^0=3 (4-1)*4^1=12 (4-1)*4^2=48  4 is not a prime!
      // How can we get 10? (11-1)*11^0
      // Wait, are these int divisions?
      N=P; R=Q; K=1;
      // R=Q? Why?. K=1 at the start too.
      
      // 100 = N=P @ R=Q @ K=1 @ GOTO 50
    }
    T+=S;//110 T=T+S @ NEXT I @ DISP T @ STOP
    //    cout << "here 110 t = " << T << " s = " << S << endl;
    if(distinct(T+1))
      {
	cout << T+1 << " solution " << I << endl;
	//	break;
      }
    
  }
  
  cout << "T="<< T << endl;
  return 0;
}

