using namespace std;

#include <iostream>
#include <vector>
#include <math.h>
#include <limits>

// k = b^e mod m using naive algorithm suitable for when e < MAXINT.
unsigned long int k(int b, unsigned long int e, unsigned long int m)
{
  long int xt=1;
  for (int n=0; n < e;n++)
    xt=(xt*b) % m;
  return xt;
}


//// kfast = b^e mod m using naive algorithm suitable for when e < MAXINT.
unsigned long long int kfast(unsigned long long int b, unsigned long long int e, unsigned long long int m)
{
  //finds a^x mod n.
  unsigned long long int f=1;
  if (m >= sqrt( std::numeric_limits<unsigned long long int>::max()))
    {
      cout << "ERROR: OVERFLOW!" << endl;
      return 0;
    }
  while(e!=0)
    {
      if(e%2!=0)
	{
	  f=(f*b)%m;
	  e=e-1;
	}
      //      cout << "b1 = " << b << endl;
      b=(b*b)%m;
      //      cout << "b2 = " << b << endl;
      e=e/2;
    }
  return f;
}



// returns the period p of (b^e== b^(e+k*p) mod m) for k>0.
// This is the multiplicative order m of b.
// Maybe return v[t] so we know where the period starts?
// Otherwise we could be assuming that b^e = b^(e+k*p) (mod m) but would be wrong because e< v[t] and k=1
int p(int b, unsigned long int m, int& vt)
{
  vector<long int> v(m,-1);
  unsigned long int n =0;
  unsigned long int t=1;
  while(true)
  {
    t = t*b % m;
    n++;
    if (v[t]==-1)
      v[t]=n;
    else
      break;
  }
  vt = v[t];
  return n-v[t];
}


int main()
{
  for (int i = 1; i <=8 ; i++)
    {
      int m = 1*pow(10,i);
      int vt;
      cout << "period of 2^e (mod 10^" << i << ") = " <<  p(2,m,vt)<< " " << vt << endl;
    }

// p3 is found from 2^(e) = 2(e+p) mod(10^m) for m = 1,2,3,4... and looking at the pattern
/*
p(2,pow(10,1)) -> 4
p(2,pow(10,2)) -> 20
p(2,pow(10,3)) -> 100
p(2,pow(10,4)) -> 500
p(2,pow(10,5)) -> 2500
p(2,pow(10,6)) -> 12500
but 
p(2,pow(10,10)) overflows my computer.  Requires a vector of size 10 billion.
 */
// if looking for the last m digits= (mod10^m) then p3 = 4*5^(m-1)
  for(  int m = 1; m <= 10 ; m++)
    {
      unsigned long int p3 = 4*pow(5,m-1);  // Euler totient function. 
      // 2^x mod 10^m = 2^x mod (2^m * 5^m).  Can take out the 2^m?
      // 2^x mod 5^m.  2^x has Order phi(5^m) = because 2^x and 2^5 are relatively prime
      // phi(a^n) = a^(n-1) * phi(a)
      // phi(5^m) = 5^(m-1) * phi(5)= 5^(m-1) * 4
      int vt2;
      int vt3;
      unsigned long int p4 = p(3,p3,vt3); // 4 and 4*5^(m-1) are not relatively prime so multiplicative order isn't defined.  Maybe need to use Chinese Remainder Theorem.
      int vt4;
      unsigned long int p5 = p(4,p4,vt4);// = 
      int vt5;
      unsigned long int p6 = p(5,p5,vt5);// = 1 // No need to go further.

      /*
      unsigned long int k7 = pow(8,9)%p8;// k(8,k8,p8);
      unsigned long int k6 = k(7,k7,p7);
      unsigned long int k5 = k(6,k6,p6);
      */
      // k4 = 5^x5 mod p4 which always equals 5^7 for x5>7
      unsigned long int k4 = k(5,m,p5); // m could be replace by m-3.... k4 works out to 5^(m-3).
      unsigned long int k3 = k(4,k4+p5,p4);
      unsigned long int k2 = k(3,k3+p4,p3);
      unsigned long int xF = k(2,k2+p3,pow(10,m));
      
      unsigned long int kf4 = pow(5,7);
      unsigned long int kf3 = kfast(4,kf4,p4);
      unsigned long int kf2 = kfast(3,kf3,p3);
      unsigned long int xfF = kfast(2,kf2,pow(10,m));

  

  cout << "m= " << m << endl
       << "p3 = " << p3 << endl
       << "p4 = " << p4 << endl
       << "p5 = " << p5 << endl
       << "p6 = " << p6 << endl
       << "vt5 = " << vt5 << endl
       << "vt4 = " << vt4 << endl
       << "vt3 = " << vt3 << endl;



  cout << "k4 = "<< k4
       << "\nk3 = "<< k3
       << "\nk2 = "<< k2
       << "\nx = "<< xF<< endl;

  cout << "kf4 = "<< kf4
       << "\nkf3 = "<< kf3
       << "\nkf2 = "<< kf2
       << "\nfx = "<< xfF<< endl;

}


  cout << std::numeric_limits<int>::max() << endl;
  cout << std::numeric_limits<unsigned int>::max() << endl;
  cout << std::numeric_limits<unsigned long int>::max() << endl;
  cout << std::numeric_limits<unsigned long long int>::max() << endl;
}

/*

p3 = 4,20,100,500,2500,12500,62500,312500,1562500,7812500,39062500,195312500,  - 4*5^(m-1) 
p4 = 2,4,20,100,500,2500,12500,62500,312500,1562500,7812500,39062500,195312500  - 4*5^(m-2)
p5 = 1,1,2,10,50,250,1250,6250,31250,156250,781250,3906250,19531250,  - 2*5^(m-3)  
p6 = 1,1,1,1,1,1,1,1,1,1,1,1,


// wolf multiplicative order(3,4*5^np4 = 2,4,20,100,500,2500,12500,62500,312500,1562500,7812500,39062500,195312500,  - 4*5^(m-2))
{{1, 4}, {2, 20}, {3, 100}, {4, 500}, {5, 2500}, {6, 12500}, {7, 62500}, {8, 312500}, {9, 1562500}, {10, 7812500}, {11, 39062500}, {12, 195312500}, {13, 976562500}, {14, 4882812500}, {15, 24414062500}}
k4 = 0,0,1,5,25,125,625,3125,15625,78125,390625,1953125,9765625,48828125 - 5^(m-3)
k3 = 0,0,4,24,124,624,3124,15624,78124,390624,1953124,9765624,48828124 - 5^(m-2) - 1
k2 = 1,1,81,481,981,3481,15981,140891,1078481,5765981,37015981,154203481,935453481,4841703481
x = 2,2,352,352,40352,340352,340352,70340352,170340352,8170340352,28170340352,928170340352,




// Python
>>> n =8**9
>>> for b in range(7, 1, -1):
...     n = pow(b, n, 10**20)
...     print("{:020d}".format(n))
... 
17825977816763596801
43557060754204000256
92256259918212890625
92256259918212890624
57555172084919828481
90356566088170340352 *** is wrong!!!
// Can just do everything mod 10^m it seems.


for m in range (3,50):
    k4=pow(5,m-3)
    k3=pow(4,k4,4*5**(m-2))
    k2=pow(3,k3,4*5**(m-1))
    x=pow(2,k2,10**m)
    print "x= {0:>{1}}".format(x,50)

x=                                                352
x=                                                352
x=                                              40352
x=                                             340352
x=                                             340352
x=                                           70340352
x=                                          170340352
x=                                         8170340352 *** 10 digits
x=                                        88170340352
x=                                        88170340352
x=                                      6088170340352
x=                                     66088170340352
x=                                    566088170340352
x=                                   6566088170340352
x=                                  56566088170340352
x=                                 356566088170340352
x=                                 356566088170340352
x=                               90356566088170340352
x=                              290356566088170340352
x=                             7290356566088170340352
x=                            87290356566088170340352
x=                           887290356566088170340352
x=                          6887290356566088170340352
x=                         96887290356566088170340352
x=                        196887290356566088170340352
x=                       4196887290356566088170340352
x=                      74196887290356566088170340352
x=                     774196887290356566088170340352
x=                    8774196887290356566088170340352
x=                   78774196887290356566088170340352
x=                  878774196887290356566088170340352
x=                 3878774196887290356566088170340352
x=                93878774196887290356566088170340352
x=               693878774196887290356566088170340352
x=              4693878774196887290356566088170340352
x=             14693878774196887290356566088170340352
x=            414693878774196887290356566088170340352
x=           3414693878774196887290356566088170340352
x=           3414693878774196887290356566088170340352
x=         203414693878774196887290356566088170340352
x=        1203414693878774196887290356566088170340352
x=       31203414693878774196887290356566088170340352
x=      731203414693878774196887290356566088170340352
x=     2731203414693878774196887290356566088170340352
x=    42731203414693878774196887290356566088170340352
x=    42731203414693878774196887290356566088170340352
x=    42731203414693878774196887290356566088170340352


x=                                                  2
x=                                                352
x=                                                352
x=                                              40352
x=                                             340352
x=                                             340352
x=                                           70340352
x=                                          170340352
x=                                         8170340352 *** 10 digits
x=                                        28170340352Wrong due to k4=5^7 not 5^(m-3)
x=                                       928170340352
x=                                      1928170340352
x=                                     81928170340352
x=                                    281928170340352
x=                                   4281928170340352
x=                                  44281928170340352
x=                                 444281928170340352
x=                                8444281928170340352
x=                               38444281928170340352
x=                              138444281928170340352
x=                             9138444281928170340352
x=                            19138444281928170340352
x=                           319138444281928170340352
x=                          8319138444281928170340352
x=                         98319138444281928170340352
x=                        998319138444281928170340352
x=                       3998319138444281928170340352
x=                      93998319138444281928170340352
x=                     793998319138444281928170340352
x=                    5793998319138444281928170340352
x=                   75793998319138444281928170340352
x=                  775793998319138444281928170340352
x=                 2775793998319138444281928170340352
x=                82775793998319138444281928170340352
x=               282775793998319138444281928170340352
x=              9282775793998319138444281928170340352
x=             69282775793998319138444281928170340352
x=            669282775793998319138444281928170340352
x=           9669282775793998319138444281928170340352
x=          69669282775793998319138444281928170340352
x=          69669282775793998319138444281928170340352
x=          69669282775793998319138444281928170340352
x=       70069669282775793998319138444281928170340352
x=      170069669282775793998319138444281928170340352
x=      170069669282775793998319138444281928170340352
x=    60170069669282775793998319138444281928170340352
x=   860170069669282775793998319138444281928170340352
x=  5860170069669282775793998319138444281928170340352


*/

