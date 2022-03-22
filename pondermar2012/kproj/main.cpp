/* Solution to IBM Ponder This March 2012
  By Chris Shannon
  Calgary Canada
*/

#include <iostream>
#include <vector>
#include <algorithm>
#include <math.h>
#include <time.h>

using namespace std;
vector<int> neighbour(vector<int> state);
int Energy(vector<int> s);
double P(double e, double enew, double T);
double temperature(double kratio);

// cache the last 10 trials to avoid repeating the same
// search for an optimum neighbour
vector< vector<int> > bestCache(10);
int bestCacheI=0;

// The number of levels, set as the optional argument to the program
// i.e. ./a.out 6
// q=3 gives 7 nodes
// q=4 gives 15 nodes
// q=5 gives 31 nodes
// q=6 gives 63 nodes
int q;

int main(int argc, char* argv[])
{
  srand (time(NULL));
  q=4;
  if (argc == 2)
    q = atoi(argv[1]);

  // the state is a top to bottom, left to right arrangement of the nodes of the tree
  // exactly as the IBM Ponder This input is required
  vector<int> state(pow(2,q)-1);
  
  double e = 1;
  double ebest = e;
  vector<int> sbest;

  int trialcount = 0;
  // repeat until a solution is found
  // typically takes 150 trials for q=6
  while (e!=0)
    {
      trialcount++;
      cout << "starting trial " << trialcount << "\n";
      if (q >=4)
	{
	  // from a ensemble of solutions to q=4 case
	  // statistics indic	ate more likely solutions to higher cases
	  // these nodes will be untouched
	  state[0]=1;
	  state[1]=pow(2,q)-2;
	  state[2]=pow(2,q)-1;
	  state[3]=2;
	  state[4]=3;
	  state[5]=5;
	  state[6]=8;
	  state[7]=4;
	  state[8]=6;
	  state[9]=7;
	  for (int i =10; i < state.size();i++)
	    {
	      state[i]=i-1;
	    }
	}
      else
	{
	  for (int i =0; i < state.size();i++)
	    {
	      state[i]=i+1;
	    }
	}
 
      e = Energy(state);   // Initial state, energy.
      sbest = state;
      ebest = e;       // Initial "best" solution
      int kmax = 1*pow(10,4);
      int emin = 0;
      int k = 1;   // Energy evaluation count.
      while (k < kmax && e > emin)	  // While time left & not good enough:
	{
	  double T = temperature(k); // Temperature calculation.
	  vector<int> snew = neighbour(state);  // Pick some neighbour.
	  double  enew = Energy(snew); // Compute its energy.
	  double probability = P(e, enew, T);
	  if (k%100==0)
	    cout << "prob = " << probability << " " << " e " << e << " enew " << enew << "\n";
	  if ( probability > rand()*1.0/RAND_MAX)// Should we move to it?
	    {                
	      state = snew; e = enew;// Yes, change state.
	    }                            
	  if (e < ebest) // Is this a new best?
	    {                              
	      sbest = snew; ebest = enew; // Save 'new neighbour' to 'best found'.
	    }                   
	  k++;                                     
	}
    }
  if (e == 0)
    {
      cout << "Success!\n";
    }
  cout << "ebest = " << ebest << "\n";
  for (int i = 0; i < sbest.size(); i++)
    cout << sbest[i] << ",";
  cout << endl;
  return 0;
}

// Probability of transition
// returns 1 if a better or equal state is found, forcing a transition
// returns lower probabilities the worse the state is
// probability is also decreased as temperature lowers (time goes on)
double P(double e, double enew, double T)
{
  if (e > enew)
    return 1;
  else
    return exp((e-enew)/T);
}

// kratio starts at 1 and goes to kmax, the value return starts at the
// max node and exponentially decays to 0
double temperature(double kratio)
{
  return pow(2,q)*pow(.99,kratio);
}

// returns a neighbouring state to the one given
// a neighbour is defined by a single swap of a pair of nodes
vector<int> neighbour(vector<int> state)
{
  int greedy = rand();
  int bestScore = 100000;
  int besti=0;
  int bestj=0;
  // Only try to find the optimum neighbour if we haven't yet done so for this State
  if (bestCache.end() == find(bestCache.begin(), bestCache.end(),state))
    {
      for (int i = 7; i < state.size() -1; i++)
	{
	  for (int j = i+1; j < state.size(); j++)
	    {
	      swap<int>(state[i],state[j]);
	      double score = Energy(state);
	      if( score < bestScore)
		{
		  bestScore = score;
		  besti=i;
		  bestj=j;
		}
	      swap<int>(state[i],state[j]);
	    }
	}
      bestCache[bestCacheI%10] = state;
      bestCacheI++;

      // Only use the best neighbour if it is a final solution, 
      // or a certain percentage of the time (half?)
      // if we always used the optimum neighbour, we'd get stuck in a local minimum
      if(bestScore == 0 || greedy%1000 < 500)
	{
	  swap<int>(state[besti],state[bestj]);
	  return state;;
	}	  
    }
  
  // else choose a random pair (excluding the perfect nodes placed in initialization)
  int i = 0;
  int j = 0;
  int size = state.size();
  while (i <= 6 || j <=6)
    {
      i = rand()%size;
      j = rand()%size;
    }
  swap<int>(state[i],state[j]);
  return state;
}

// returns the sum of the square of the edges used more than once or not at all
// returns 0 if all edges are used exactly once
int Energy(vector<int> s)
{
  vector<int> edges;
  int duplicates = 0;
  vector<int> edgeDiffs(s.size()-1,0);
  for (int i = 1; i < s.size(); i++)
    {
      int parent = (i-1)/2; // handy index
      edgeDiffs[abs(s[i]-s[parent])-1]++;
    }

  int energy = 0;
  // suppose we could have alternatively initialized edgeDiffs all to -1...
  for(int i = 0; i < edgeDiffs.size(); i++)
    {
      energy += pow(edgeDiffs[i]-1,2);
    }
  return energy;
}

