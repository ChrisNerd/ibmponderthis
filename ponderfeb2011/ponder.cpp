#include <iostream>
#include <math.h>
#include <vector>
using namespace std;
bool conditionA(long int n);
bool conditionB(long int n);
bool conditionC(long int n);
bool isFlippable(long int n);
long int flip(long int n);
vector<int> nAsVector(long int n);

/*
solution 5128050
solution 9902164
solution 11205292
solution 15102610
solution 16188550
k 10000
solution 22595596
solution 28560222
solution 29260050
k 20000
solution 50102054
solution 50110098
solution 50295110
solution 51280500
*/

/*
solution 100
k 0
solution 502850
solution 905050
solution 925160
solution 1608900
solution 5099996
solution 10815258
solution 10960050
solution 11225502
solution 16088100
*/

int main()
{
for (long int k = 0; k < pow(10,12);k++)
  {
    long int n = k*2011;// + 100;
    if (conditionA(n) && conditionB(n) && conditionC(n))
      {
	cout <<  "solution "<< n <<endl;
      }
    
    if (k%10000000 == 0)
      {
	cout << "k " << k <<endl;
      }
  }
 return 0;
}
bool conditionA(long int n)
{
  if (!isFlippable(n))
    {
      return false;
    }

  if ( n == flip(n))
    {
      return true;
    }
  return false;
}

bool conditionB(long int n)
{
  return isFlippable(n*n);
}

bool conditionC(long int n)
{
  // because n = k*2011
  return true;
}

bool isFlippable(long int n)
{
  vector<int> nVec = nAsVector(n);
  for(int i = 0; i < nVec.size(); i++)
    {
      //      cout << "nVec[" << i << "] = " << nVec[i] << " ";
      if (nVec[i] == 3 ||  nVec[i] == 4 || nVec[i] == 7)
	{
	  return false;
	}
    }
  return true;
}

long int flip(long int n)
{
 n = 8081808811;

  long int flipped = 0;
  vector<int> nVec = nAsVector(n);
  
  for (int i = 0; i < nVec.size(); i++)
    {
      int nVecI = 0;
      if ( nVec[i] == 0 )
	{
	  nVecI = 0;
	}
      if ( nVec[i] == 1 )
	{
	  nVecI = 1;
	}
      if ( nVec[i] == 2 )
	{
	  nVecI = 5;
	}
      if ( nVec[i] == 5 )
	{
	  nVecI = 2;
	}
      if ( nVec[i] == 6 )
	{
	  nVecI = 9;
	}
      if ( nVec[i] == 8 )
	{
	  nVecI = 8;
	}
      if ( nVec[i] == 9 )
	{
	  nVecI = 6;
	}
      flipped += nVecI * pow(10,i);
     }
  return flipped;
}

vector<int> nAsVector(long int n)
{
  long int numInts = log10(n);
  vector<int> integers;
  while (numInts >= 0)
    {
      integers.push_back(n/pow(10,numInts));
      n -= integers.back() * pow(10,numInts);
      numInts--;
    }
  return integers;
}
