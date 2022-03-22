// Chris Shannon
// Calgary Canada
// For IBM Ponder This July 2012
// This generates a matrix of transition probabilities for a markov chain
// the states are the partitions of the input variable.
// The output is to be used in Octave to compute the exact expented value
// of Alice's game with a given number of balls.
// g++ markov.cpp && ./a.out 10
#include <iostream> 
#include <vector>
#include <math.h>
#include <algorithm>
#include <time.h>
#include <functional>
using namespace std;

vector<vector<int> > states;
int numBalls = 8;

//states[0] = 1,1,1,1,1,1,1,1;
//states[1] = 2,1,1,1,1,1,1;

int sum(vector<int>& v, int d)
{
  int r=0;
  for (int i=0; i <= d;i++)
    r+=v[i];
  return r;
}

// Print the states of the Markov chain
// the states correspond to the partitions of the number of balls
// for example if N=3, the partitions are 1,1,1; 2,1; and 3.
void pr(vector< vector<int> >& v)
{
  for (int i=0;i < v.size() ; i++)
    {
      for (int j=0; j < v[i].size(); j++)
	cout << v[i][j] << " ";
      cout << endl;
    }
  cout << endl;
}


// recursive function to generate the partitions
// numbers are recursively added to a vector until they sum to numBalls
// then that vector is added to states
int a(int depth, vector<int>& s)
{
  for (int x=1;x<=numBalls;x++)
    {
      s.resize(depth);
      if (depth==0)
	{
	  s.resize(depth);
	  s.push_back(x);
	}
      else if (x>s[depth-1])
	break;
      int summation = sum(s,depth-1);
      if (summation+x > numBalls)
	break;
      else if (summation+x < numBalls)
	{
	  s.resize(depth);
	  s.push_back(x);
	  a(depth+1,s);
	}
      else if (summation+x == numBalls)
	{
	  s.resize(depth);
	  s.push_back(x);
	  states.push_back(s);
	}
    }
}

// decrecated function that grouped like terms to exploit symmetry
// turns out that this wasn't a bottleneck and wasn't worth the effort to optimize
void group(vector<int>& r, vector<int>& ga, vector<int>& gb)
{
  // r is a row like 4 2 1 1
  // ga will be 4 2 1 (unique entries)
  // gb will be 1 1 2 (cardinality)
  //  cout << r.size() << " " << ga.size() << " " <<gb.size()<<endl;
  for (int i =0; i < r.size(); i++)
    {
      int p=-1;
      for (int j=0;j<ga.size();j++)
	{
	  if (ga[j]==r[i])
	    {
	      p=j;
	      break;
	    }
	}
      if (p==-1)
	{
	  ga.push_back(r[i]);
	  gb.push_back(1);
	}
      else
	{
	  gb[p]++;
	}
    }
}

// Search function that returns the index of the state s
int findState(vector<int>& sn)
{
  vector<int> s;
  // remove entries that contain 0 since empty piles are disregarded.
  for (int i =0; i < sn.size(); i++)
    {
      if (sn[i]!=0)
	s.push_back(sn[i]);
    }
  sort (s.begin(), s.end(), greater<int>( ) );
  int j;
  for (j=0; j < states.size(); j++)
    {
      if (states[j]==s)
	break;
    }
  return j;
}

int main( int argc, char *argv[] )
{
  if (argc == 2)
    {
      numBalls= atoi(argv[1]);
    }

  // Generate the list of states.
  vector<int> s;
  a(0,s);
  pr(states);

  // for each state, find all possible transistions
  // and count their frequencies

  double mat[states.size()][states.size()];
  for (int i=0; i < states.size(); i++)
    {
      for(int j =0; j<states.size(); j++)
	{
	  mat[i][j]=0;
	}
      
      for (int k=0; k < states[i].size(); k++)
	{
	  // the probability of the first ball being of colour k
	  double p1 = states[i][k] *1.0/ numBalls;
	  vector<int> snew=states[i];
	  // remove 1 ball of colour k
	  snew[k]--;
	  for (int m=0; m < states[i].size(); m++)
	    {
	      // can't remove two balls of a particular colour if there was only one.
	      if (states[i][k]==1 && m==k)
		continue;
	      // probability that the second ball is of colour m
	      double p2 = snew[m] *1.0/ (numBalls-1);
	      // the first ball gets painted the colour of the second
	      snew[m]++;
	      int stateIndex= findState(snew);
	      // the markov transition probability from state i to this new state is increased by the product of the two probabilities
	      mat[i][stateIndex]+=p1*p2;
	      // reset for the next round
	      snew[m]--;
	    }
	}

    }
  cout << "format long" << endl << "p=[";
  cout.precision(15);
  for (int i=0; i < states.size(); i++)
    {
      cout << "[";
      for (int j=0; j < states.size(); j++)
	{
	  cout << mat[i][j];
	  if (j != states.size()-1)
	    cout << " ";
	}
      cout << "]";
      if (i!=states.size()-1)
	cout << endl;
    }
  cout << "];" <<endl;
  cout << "inv(eye(" << states.size()-1 << ")-p(1:"<< states.size()-1 << ",1:" << states.size()-1 << ")) * ones(" << states.size()-1 << ",1)" << endl;


  /*      vector<int> ga;
      vector<int> gb;
      group(states[i],ga,gb);
      for (int k=0; k <ga.size();k++)
	{   
	  cout << ga[k] << " " << gb[k]<<endl;
	  // the probabibity that group k was choose as the first ball
	  double p1 = ga[k]*gb[k]/8;
	  vector<int> stemp = states[i];
	  int dum=0;
	  for (; dum < stemp.size(); dum++)
	    {
	      if (stemp[dum] == ga[k])
		break;
	    }
	  stemp[dum]--;
	  vector<int> ga2;
	  vector<int> gb2;
	  group(stemp,ga2,gb2);
      // there is a ga[0]*gb[0]/8 chance that the first ball picked is from the first group
      // if the first ball is taken from the ga[0] group, then gb[0] is reduced by 1
      // i.e. gb[0]--
      // it will be placed in gb[0] with probability ga[0]*gb[0]/8
      // it will be placed in gb[0] with probability ga[1]*gb[1]/8
      // ...
	  for(int m=0; m< ga2.size(); m++)
	    {
	      double p2 = p1*ga2[m]*gb2[m]/7;
	      
	      
	      // ga=4,2,1
	      // gb=1,1,2
	      // 3,2,1,1 regroups to 
	      // ga2= 3,2,1
	      // gb2= 1,1,2
	      // 4,2,0,1, regroups to
	      // 4,2,1
	      // 1,1,1
	      findState(

	    }

	}
    }
	  */

}
