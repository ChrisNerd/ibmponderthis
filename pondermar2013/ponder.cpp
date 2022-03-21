#include <iostream>
#include <vector>
#include <math.h>
#include <stdlib.h>
#include <algorithm>
#include <time.h>

using namespace std;

/*
Probably going about this all wrong.  Could try using an integer solver
Could try reasoning about it, saying that purchases of abcd must be indistinguishable from a purchase ab for some ab.


 */

/*
  [bcde] is a vector made of elements that can be 1,2,3,4 or 5.
  5^4 = 625 such vectors.
  of those, some are componsed of just 1, some of just 2, ... elements.
  e.g. 1111 is just 1 element, 1222 is 2 elements, and 1233 is 3 elements.
  Mark which are 2 elements
  There are 140 sets with exactly 2 elements
*/

void createV(vector<vector<int> >& v, int count)
{
  for (int i = 1; i < v.size(); i++)
    {
      bool carry=true;
      int c=count-1;
      while(c >=0)
	{
	  if (carry)
	    {
	      if (v[i-1][c] == 4)
		{
		  v[i][c] == 0;
		  carry = true;
		}
	      else
		{
		  v[i][c]=v[i-1][c]+1;
		  carry = false;
		}
	    }
	  else
	    {
	      v[i][c] = v[i-1][c];
	    }
	  c--;
	}
    }
}

int errorFunction(vector<int>& ne, vector<vector<int> >& v, vector<int>& A,   vector<vector<int> >& P)
{
  vector<int> bi(v.size());
  for(int c=0; c< v.size(); c++)
    {
      int bout=0;
      for (int i = 0; i < A.size(); i++)
	{
	  bout=P[A[i]][0]*1;
	  for( int j =0; j < 4; j++)
	    {
	      bout+=P[A[i]][j+1]*v[c][j];
	    }

	  if(bout%5 != 0)
	    {
	      bi[c]++;
	    }
	}
      //at the end of the month, is whether the cumulative number of points you were fined is an integer or not.
      bi[c] = bi[c]%2;
    }


  int errordiff=0;
  for (int i = 0; i < bi.size(); i++)
    {
      if (bi[i] != ne[i])
	{
	  errordiff++;
	}
    }
  //  cout << "error " << errordiff << endl;
  errordiff = min(errordiff, 625-errordiff);

  if (errordiff ==0)
    {
      for (int i = 0; i < A.size(); i++)
	{
	  cout << A[i] << endl;
	}
      exit(0);
    }


  return errordiff;
 
}

void neighbour(vector<int>& A, vector<int>& B,int& i, int& j)
{
  i = rand()%A.size();
  j = rand()%B.size();
  swap(A[i],B[j]);
}


void Metropolis(vector<int>& A, vector<int>& B, vector<vector<int> >& v, vector<int>& ne, vector<vector<int> >& P, double temperature, double M)
{

  // store the best
  vector<int> Aorig = A;
  vector<int> Borig = B;
  vector<int> Abest = A;
  vector<int> Bbest = B;
  int besterror = errorFunction(ne, v, A, P);
  int origerror = besterror;
  bool swapped = false;
  int eold = besterror;
  int e1;
  int i,j;
  while (M > 0)
    {
      if(swapped)
	{
	  eold=e1;
	}
      else
	{
	  // unnecessary
	  //	  eold=errorFunctionBruteForce(People);
	}
      neighbour(A,B,i,j);

      e1 = errorFunction(ne, v, A, P);

      double deltah= e1 - eold;
      double edt = exp(-deltah/temperature);
      //      cout << deltah << " " << temperature << " " << e1 << " " << edt << " " << endl;
      
      if ( deltah < 0 || (double) rand() / RAND_MAX < exp(-deltah/temperature))
	{
	  //  cout << "swapped "<< i << " " << j << endl;
	  //  peopleMap=newpeopleMap;
	  // keep the swap
	  swapped = true;
	}
      else
	{
	  //	  	  cout << "revert"<<endl;
	  // revert
	  swap(A[i], B[j]);
	  swapped = false;
	}
      if (e1 < besterror)
	{
	  besterror = e1;
	  Abest=A;
	  Bbest=B;
	}	  
      M--;
    }
  // Return the best
  if (besterror < eold)
    {
      A=Abest;
      B=Bbest;
    }

  cout <<eold << " " << besterror << " " << origerror << endl;
}

void simulatedAnnealing(vector<vector<int> >& v, vector<int>& ne )
{
  // 31 distinct ints from 0 to 625.
  vector<int> B;
  for (int i =0;i<625;i++)
    {
      B.push_back(i);
    }
  vector<int> A;
  for (int i =0; i < 31; i++)
    {
      int r = rand() % B.size();
      A.push_back(B[r]);
      B.erase(B.begin() + r);
    }

  cout << A.size() << " " << B.size() << endl;

  vector<vector<int> > P(pow(5,5), vector<int> (5));
  createV(P,5);

  double T0=1000; // initial temperature
  double alpha = .90; // cooling rate
  double beta = 1.05; // increases the time spent in Metropolis after each temperature decrease
  double maxtime = 10000000; // max time for entire algorithm
  double M = 1000; // time until the next parameter update

  double temperature=T0;
  double Time = 0;

  /*
int      e1 = errorFunction(ne, v, A, P);
      cout << "Here 1 " << e1 << endl;
      Metropolis(A, B, v, ne, P, temperature, M);
      int      e2 = errorFunction(ne, v, A, P);
      cout << "Here 2 " << e2 << endl;
      */

        while (Time < maxtime)
    {
      Metropolis(A, B, v, ne, P, temperature, M);
      Time += M;
      temperature *= alpha;
      M *= beta;
      }
}

// Return the number of distinct elements in the vector v
int numElements(vector<int>& v)
{
  vector<int> t;
  int numdistinct=0;
  for(int i =0;i<4;i++)
    {
      int vi = v[i];
      bool inthere = false;
      for (int j=0; j<t.size();j++)
	{
	  if (vi == t[j])
	    {
	      inthere = true;
	      break;
	    }

	}
      if (!inthere)
	{
	  numdistinct++;
	  t.push_back(vi);
	}
    }
  return numdistinct;
}

int main()
{
  srand (time(NULL));
  // v[i] is something like 2342
  vector<vector<int> > v(pow(5,4), vector<int> (4));
  createV(v,4);

  // nE[i] contains the number of distinct elements of v[i]
  // ne[i] is 0 if v[i]!=2 and ne[i] is 1 if v[i]==2
  vector<int> nE;
  for (int i =0; i < v.size();i++)
    {
      cout << i << " ";
      for (int j=0; j < v[i].size(); j++)
	{
	  cout << v[i][j] << " ";
	}
      nE.push_back(numElements(v[i]));
      cout << numElements(v[i]) << endl;
    }

  cout << "Numbers with 2 elements, there are " << endl;
  int ne2=0;
  vector<int> ne(v.size());
  for (int i = 0; i < nE.size(); i++)
    {
      if (nE[i]==2)
	{
	  ne2++;
	  for (int j=0; j < v[i].size(); j++)
	    {
	      cout << v[i][j] << " ";
	    }
	  cout << endl;
	  ne[i]=1;
	}
    }
  cout << ne2 << endl;

  // A purchase is another set Pi = [pa pb pc pd pe] where each element
  // is 0,1,2,3,4.  There's no point in it being 5.
  // There are 5^5 = 625 possible purchases
  // Choose 31 of theme  625 choose 31 ~= 2*10^52

  // The cost of a purchase Pi is a boolean bi. 0 if Pi*v mod 5 == 0, 1 otherwise
  // The feedback you get after all Purchases is a boolean = all the bi's xor'ed with each other

  // Want that feedback to map exactly to the set ne2.
  // Error function could be the number of incorrect classifications
  // Need a way to update P.  K.  Start with a random set A of 31 from the possible 625.  The remaining 594 is set B.  Swap a member from A and B.
  simulatedAnnealing( v, ne );
}
