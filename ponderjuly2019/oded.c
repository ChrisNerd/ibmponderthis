#include <iostream>
using namespace std;
int main()
{
int m=0;
int k=0;
int n=7;
double j=0;
//double a[5] = {1000.0, 2.0, 3.4, 7.0, 50.0};
//double a[7] = {0.0, 0.0, 0.0, 1.0, 1.0, 0.0, 0.0}; // m=3
// double a[7] = {0, 1, 2, 3, 4, 5, 6}; // m=1
// double a[7] = {0, 0, 0, 0, 4, 5, 6}; // m=1
// double a[7] = {0, 1, 0, 1, 0, 1, 0}; // m=1
// double a[7] = {1, 0, 0, 1, 0, 1, 0}; // m=1
// double a[7] = {0, 1, 1, 1, 0, 1, 0}; // m=1
 double a[7] = {0, 1, 1, 1, 0, 1, 1}; // m=3

 // a.groupBy(identity).mapValues(_.size).maxBy(_._1) //(Int, Int) = (1,5)
 
//It computes something when the array a has a special feature...
// So, j and m are the only things computed.
// j is a value from a,
// and m is a count of j.
for(m=k=0; k<n; m+= !(a[k++]-j)<<1 )
  {
    cout << "m " << m << " n " << n << " k " << k << endl;
    for (int ia = 0; ia < n; ia++)
      cout << a[ia] << " ";
    cout << endl;
    if(!m--)
      {
	cout << "m " << m << " j " << j << endl;
	j=a[k];
	cout << " j " << j << endl;
      }
    // So it does the ! before the <<
    // So !0 is 1, then shift left by 1, so it's 10 in binary, which is 2.
    // And !anythingElse is 0, then <<1 is still 0.
    // So !x<<1 will result in 2 if x==0 and 0 otherwise.
    // j is a[k] at the last time m==0.
    // m is decremented every iteration of the for loop, but also either += 0 or 2.
    // so net it is either increased by +1 or decreased by 1 m+=1 or m-=1.
    // If a[k]==j, then m++, otherwise m--.
    // If m==0, then j=a[k], and m gets incremented.
    // If m!=0, then we could still get a[k]==j,
    // So is m going to be the max count??? The count of the "mode"?
    // There's something going on with the parity of the index, k. m can only be 0 on even k.
    
    cout << "a[k] " << a[k] << " a[k] -j " << a[k] -j << " !(a[k]-j) " << !(a[k]-j) << " !(a[k]-j)<<1 " << (!(a[k]-j)<<1) << endl;
    
      

  }
 cout << "m at end " << m << endl;
}


// I think this finds the value of the element that is present in more than 50% of the array, or returns m==0 if none exists.
