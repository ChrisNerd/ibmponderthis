// Chris Shannon
// Calgary Canada
// For IBM Ponder This July 2012
// Monte Carlo simulation of determine the expected number of
// trials to complete Bob's game.

#include <iostream> 
#include <vector>
#include <math.h>
#include <algorithm>
#include <time.h>
using namespace std;


int main()
{
  srand (time(NULL));
  int numRuns=100;
  int maxUrns=200;
  for (int numUrns=2; numUrns <= maxUrns; numUrns++)
    {
      int trials = 0;

      for (int run=0; run < numRuns; run++)
	{
	  int successes=0;
	  vector<bool> urns(numUrns,false);    
	  while(1)
	    {
	      trials++;
	      int r = rand() % numUrns;
	      if (urns[r] == false)
		{
		  successes++;
		}
	      urns[r]=true;
	      if (successes == numUrns)
		break;
	    }
    
	}
      cout << "numUrns = " << numUrns << " experned trials = " << trials*1.0/numRuns << endl;
    }
}
