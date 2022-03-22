#include <iostream>
#include <math.h>
#include <gmp.h>
#include <gmpxx.h>

using namespace std;
int main()
{
  mpf_class a (1000), b (1000), c (1000), p2 (1000), bestS;
  mpz_class aZ(1000);
  a.set_prec(1000);
  b.set_prec(1000);
  c.set_prec(1000);
  p2.set_prec(1000);
  bestS.set_prec(1000);  

  a = "61031369";
  b = "122085875";
  c = a/b;
  p2="0.499905242928389504529621945225162859221090463453651027277789301914";
  c=p2-c;
  bestS = "100";
  cout << "quotient is " << c << "\n";
  cout << "absolute value is " << abs(c) << "\n";
  
  for ( long int i1 = 1; i1 < pow(10,9); i1++)
  {
    a=p2*i1;
    aZ=a;
    //    a=n;
    b=i1;
    c=aZ/b;
    c=abs(p2-c);
    if(c < bestS)
      {
	bestS=c;
	cout << aZ << " " << b << " " << c << endl;
      }
    aZ++;
    c=aZ/b;
    c=abs(p2-c);
    if(c < bestS)
      {
	bestS=c;
	cout << aZ << " " << b << " " << c << endl;
      }

  }




long double p = 0.499905242928389504529621945225162859221090463453651027277789301914;
//              0.4999052429283895022207647397596527235306223494178540
long double bestSquared=110;
long int bestp=1;
long int bestn=1;
 for (long int i=1; i < pow(10,9); i++)
   {
long int n=p*i;
long double pstar=1.0d*n/i;
 if((pstar-p)*(pstar-p)==0)
  {
  bestp=i;
bestn=n;
 bestSquared=(pstar-p)*(pstar-p);
 a=n;
 b=i;
 c=a/b;
 c=p2-c;
 cout << bestn << " " << bestp << " " << bestSquared << " " << c << endl;
   }
  }

 cout << bestn << " " << bestp << " " << bestSquared <<endl;
}
// wolf (1-q)*(1+q^13+q^14+q^18+q^19+q^22)=1/2


// p*(1+(1-p)^13+(1-p)^14+(1-p)^18+(1-p)^19+(1-p)^22)=1/2
// -1/2 + 6 p - 86 p^2 + 724 p^3 - 3975 p^4 + 15967 p^5 - 49819 p^6 + 125028 p^7 - 257904 p^8 + 443400 p^9 - 641135 p^10 + 784069 p^11 - 813280 p^12 + 715702 p^13 - 533135 p^14 + 334459 p^15 - 175236 p^16 + 75735 p^17 - 26523 p^18 + 7335 p^19 - 1541 p^20 + 231 p^21 - 22 p^22 + p^23 == 0
//
// 
// denominator numerator (p-n/d)
// 61031369 122085875          1.38579e-17
// 122062738 244171750 = 2

// 183094107 366257625 = 3
// 244125476 488343500 = 4
// 305156845 610429375 = 5
// 332550663 665227396  -5.32570209849165777253774428176380976354606887... × 10^-17

/*
// 338794396 677717229    -2.88579083281124801476383382201869119898976232... × 10^-17
*/

// 366188214 732515250 = 6
// 393582032 787313271    -5.13476852467680481887862534899801022823371327... × 10^-17
//    399825765 799803104  -3.07027894558688744052304363463247716381086156... × 10^-17
// 427219583 854601125 = 7
// 454613401 909399146 -4.99510021216903343321519042941200491793859191... × 10^-17
// 460857134 921888979      -3.20590348978023531471287370465246410551140536... × 10^-17
// 488250952 976687000 = 8 =  -4.09440047130594257551456411506663280448302657... × 10^-17
// 94668920  189373729 0 2.30886e-18  verified in Wolfram
// 1 a
// 14 n
// 15 o
// 19 s
// 20 t
// 23 w
// p = 0,2,2637,1,4,1,2,1,2,1,1,2,0,0,0,1,1 = 1012925/2026234
// 0 + 1/(2 + 1/(2637 + 1/(1 + 1/(4 + 1/(1 + 1/(2 + 1/(1 + 1/(2 + 1/(1 + 1/(1 + 1/2))))))))))

//[0; 2, 2637, 1, 4, 1, 2, 1, 2, 1, 1, 2, 1, 1, 2, 4, 1, 1, 1, 11, 2, 3, 4, 1, 94, 1, 4,
//1, 1, 2, 2, 4, 1, 18, 3, 174, 1, 2, 3, 1, 1, 1, 2, 23, 10, 1, 5, 1, 1, 1, 1, 4, 1, ...]

// fromcontinuedfraction[continuedfraction[0.499905242928389504529621945225162859221090463453651027277789301914,19]]
// 94668920   /189373729 19
// 1102389489/2205196894 20
// 4010795079052/8023110651045 25
