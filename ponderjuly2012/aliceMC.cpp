// Chris Shannon
// Calgary Canada
// For IBM Ponder This July 2012
// Monte Carlo simulation of determine the expected number of
// trials to complete Alice's game.

#include <iostream> 
#include <vector>
#include <math.h>
#include <algorithm>
#include <time.h>
using namespace std;

bool allSame(vector<int>& urn)
{
  for (int i =0; i < urn.size()-1;i++)
    {
      if (urn[i] != urn[i+1])
	return false;
    }

  return true;
}


int main()
{
  srand (time(NULL));
  int numRuns=10000;
  int maxUrns=100;
  for (int numUrns=2; numUrns <= maxUrns; numUrns++)
    {
      int trials = 0;
      for (int run=0; run < numRuns; run++)
	{
	  vector<int> urns(numUrns);
	  for (int i = 0; i < numUrns; i++)
	    urns[i]=i;

	  while(1)
	    {
	      trials++;
	      int r1,r2;
	      // Alice must pick two unique balls
	      do
		{      
		  r1 = rand() % numUrns;
		  r2 = rand() % numUrns;
		}
	      while (r1==r2);
	      urns[r1]=urns[r2];
	      if (allSame(urns))
		{
		  break;
		}
	    }
	}
      cout << "numUrns = " << numUrns << " experned trials = " << trials*1.0/numRuns << endl;
    }
}
