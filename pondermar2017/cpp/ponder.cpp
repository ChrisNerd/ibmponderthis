#include  <iostream>
#include <stdio.h>      /* printf, scanf, puts, NULL */
#include <stdlib.h>     /* srand, rand */
#include <time.h>
#include <algorithm>
#include <math.h>
#include <string.h>
using namespace std;
double initialTemperature = 2.0;
double finalTempurature=0.0001;
double coolingRate = 0.005;
//3^3=27
//3^4=81
//3^5=243
//3^6=729
const int N=5;
const int len=pow(3,N);//243;
int allV [len][N];


void createAllV(){

  for (int i =0; i< N; i++)
    allV[0][i] =0;
  bool overflow = false;
  for (int i = 1; i < len ; i++)
    {
      if ( allV[i-1][N-1] == 2 )
	{
	  overflow = true;
	}
      else
	{
	  overflow = false;
	}
      allV[i][N-1] = (allV[i-1][N-1] + 1)%3;
      
      for (int j = N-2; j >= 0; j--)
	{
	  
	  if ( allV[i-1][j+1] == 2 && overflow )
	    {
	      allV[i][j] = (allV[i-1][j]+1)%3;
	    }
	  else
	    {
	      overflow = false;
	      allV[i][j] = allV[i-1][j];
	    }
	}
    }
}



int score(int S[N][N])
{
  int sums[len] = {};
  for (int i = 0; i < len; i++)
    {
      int* v = allV[i];
      for (int r =0; r < N; r++)
	{
	  int countArray[3] = {};
	  for (int j=0; j<N; j++)
	    countArray[ (v[j]+S[r][j] )%3]++;
	  sums[i]+= *std::max_element(countArray,countArray+3);
	}
      /*
      //      cout << "sum 7"<<endl;
      */
    }
  return *std::max_element(sums,sums+len);
}

void printm(int S[N][N]){
  for (int i =0; i < N ; i++)
    {
      for (int j =0; j < N ; j++)
	cout << S[i][j] << " " ;
      cout << endl;
    }
}

void greedy(int S[N][N])
{
  //  printm(S);
  int bestScore = score(S);
  //  cout << bestScore << endl;
  for (int i = 0; i< N; i++)
    for (int j = 0; j< N; j++)
      for (int inc = 1; inc <= 2; inc++)
	{
	  S[i][j]=(S[i][j]+inc)%3;
	  int s = score(S);
	  if (s < bestScore)
	    {
	      greedy(S);
	      return;
	    }
	  else
	    S[i][j]=(3+S[i][j]-inc)%3;
	}
}

void simulatedAnnealing(int S[N][N], double temp) {
  bool accept = false;
  int currentEnergy = score(S);
  int neighbourEnergy;
  while (temp > finalTempurature){
    if (accept)
      currentEnergy = neighbourEnergy;
    //    else
    //currentEnergy = score(S);
    //    cout << currentEnergy << " " << temp << endl;
    //    val neighbour = createNeighbour( best );
    int r =  rand() % N;
    int v =  rand() % N;
    int inc =  1 + rand() % 2;
    S[r][v] = (S[r][v] + inc )%3;
    neighbourEnergy = score(S);
    if (neighbourEnergy < 37)
      {
	printm(S);
	return;
      }
    else
      {   // Decide if we should accept the neighbour
	accept = (exp((1.0*currentEnergy - neighbourEnergy)/temp) > (double)rand() / RAND_MAX);
	if (!accept)
	    S[r][v] = (3+S[r][v] - inc )%3;
	temp *=1-coolingRate;
      }
  } 
}

bool isSym(int S[N][N]){
  bool sym = true;
  for (int ii = 0; ii< N; ii++)
    {
      for (int jj=0; jj <N; jj++)
	{
	  if (S[ii][jj] != S[jj][ii])
	    sym = false;
	  
	  //  cout << (S[ii][jj]+v[jj])%3 << " ";
	}
      //	  cout << endl;
    }
  return sym;
}

  
// Find the best NxN matrix
void findBest(){
  int bestS[N][N];
  int bestScore = 100;

  int S[N][N];
  for (int i =0; i <N;i++)
    for (int j =0; j <N;j++)
      S[j][i]=0;

  for (int i =1; i <N-1;i++)
    S[i][N-1-i]=1;
  

      bool overflow = false;
  while (true)
    {
      //      printm(S);
      bool all2s = true;

      if (overflow == true)
	break;

      for (int i =1; i <N;i++)
	//	for (int j =0; j <N;j++)
	for (int j =1; j <N;j++)
	  if (S[i][j] != 2)
	    {
	      all2s = false;
	      break;
	    }
      
      if (all2s == true)
	break;
      int sc = score(S);
      if (sc < bestScore){
	bestScore = sc;
	for (int i =0; i <N;i++)
	  for (int j =0; j <N;j++)
	    bestS[j][i] = S[j][i];
      }

      if (sc ==15 && isSym(S))
	{
	  cout << "Found 10"<<endl;
	  printm(S);	  
	}
      
      if ( S[N-1][N-1] == 2 )
	{
	  overflow = true;
	}
      else
	{
	  overflow = false;
	}
      S[N-1][N-1] = (S[N-1][N-1] + 1)%3;
      for (int i = N-1; i >= 0; i--) //      for (int i = N-1; i >= 1; i--)
	{
	  if (!overflow)
	    break;
	  for (int j = N-1; j >= 0; j--)//	  for (int j = N-1; j >= N-i; j--)//	  for (int j = N-1; j >= 0; j--)
	    {
	      if (i == N-1 && j == N-1) // Bottom right
		continue;
	      else if (j == N-1) // right column but not on bottom
		{
		  // Next row down, leftmost
		  if ( S[i+1][0] == 0 && overflow )//  if ( S[i+1][N-i-1] == 0 && overflow )//	if ( S[i+1][0] == 0 && overflow )
		    {
  
			  S[i][j] = (S[i][j]+1)%3;
			  if (i == 1 && S[i][j] != 0)
			      overflow = false;
		    }
		  else
		    {
		      overflow = false;
		      break;
		    }
		}
	      else
		{
		  // any where but the rightmost column
		  if ( S[i][j+1] == 0 && overflow )
		    {
		      S[i][j] = (S[i][j]+1)%3;
		    }
		  else
		    {
		      overflow = false;
		      break;
		    }
		}
	    }
	}
    }
  cout << "Best Score "<< bestScore << ". Best Matrix "<< endl;
  printm(bestS);
  score(bestS);
  cout << "end best score"<< endl;
  
  /*
N=3
Best Score 6. Best Matrix 
0 0 0 
0 1 2 
0 2 1


0 1 1 
0 2 0 
0 0 2 

 
0 2 2 
0 0 1 
0 1 0


N=4
Best Score 10. Best Matrix 
0 0 0 0 
0 0 1 1 
0 1 0 2 
0 2 1 2 

0 0 0 0 
0 1 0 2 
0 0 1 1 
0 2 1 2 

0 0 0 0 
0 1 1 0 
0 1 2 2 
0 0 2 1 

0 0 0 0 
0 1 1 2 
0 1 2 0 
0 2 0 2

0 0 0 0 
0 1 2 0 
0 2 2 1 
0 0 1 1 


0 0 0 0 
0 1 2 1 
0 2 2 0 
0 1 0 2 

0 0 0 0 
0 2 1 0 
0 1 1 2 
0 0 2 2 

0 0 0 0 
0 2 2 1 
0 2 1 0 
0 1 0 1



N=5
Best Score 15. Best Matrix 
0 0 0 0 0 
0 0 0 1 2 
0 0 1 2 1 
0 1 2 0 0 
0 2 1 0 1


0 0 0 0 0 
0 0 0 1 2 
0 0 1 2 1 
0 1 2 1 2 
0 2 1 2 2 (but not 1)

0 0 0 0 0 
0 0 0 1 2 
0 0 2 2 1 
0 1 2 1 1 
0 2 1 1 2 

Found 10
0 0 0 0 0 
0 0 0 1 2 
0 0 2 2 1 
0 1 2 2 0 
0 2 1 0 0 

arrow head matrix
Found 10
0 0 0 0 0 
0 0 1 2 1 
0 1 0 2 2 
0 2 2 0 1 
0 1 2 1 0 


0 0 0 0 0 
0 0 1 2 2 
0 1 0 1 2 
0 2 1 0 1 
0 2 2 1 0 

0 0 0 0 0 
0 1 1 0 2 
0 1 1 2 0 
0 0 2 1 0 
0 2 0 0 1 



N=6
Best Score 22. Best Matrix 
0 0 0 0 0 0 
0 0 0 0 1 0 
0 0 0 1 0 1 
0 0 1 2 2 2 xxx 3 2s! Add 1 to this row.
0 1 2 0 1 2 
0 2 1 0 2 1 


Donna says
N=6
0 0 0 0 0 0
0 0 0 0 2 1
0 0 0 1 1 2
0 0 
0

Triangular numbers.
3, 4, 5, 6, 7, 8
6,10,15,21,28,36

Looks like the pattern is second symmetric, m=m transposed. (expect for N=4, there's a bit of a swap going on).
Second row starts with N-2 zeros
N=8 should be something like

0 0 0 0 0 0 0 0
0 0 0 0 0 0 1 x1
0 0 0 0 0 1 2 3
0 0 0 0 1 4 5 6
0 0 0 1 7 8 9 10
0 0 1 1 2 3 4 5
0 1 6 7 8 9 0 1
0 2 3 4 5 6 7 8

18 variables.

What about N=6?

0 0 0 0 0 0
0 0 0 0 1
0 0 0 1
0 0 1
0 1
0 -- best we can do with this guess is 22.

How about 
0 0 0 0 0 0
0 0 1 2 3 4
0 1 0 5 6 7
0 2 5 0 8 9
0 3 6 8 0 10
0 4 7 9 100??
3^10=59049

Lets try iterating through all these matrices, see if we can get score==21!

  */
  
}





int main(int argc, char* argv[])
{
   srand (time(NULL));
   /*
   // allV looks perfect
   for (int i = 0; i < 6561 ; i++)
    {
      for (int j = 0; j < N; j++)
	{
	  cout << allV[i][j] << " " ;
	}
      cout << endl;
    }
  */

   createAllV();

   /*   int t4[4][4] ={{0, 0, 0 ,0}, 
		  {0, 0, 1, 1 },
		  {0, 1, 0, 2 },
		  { 0, 2, 1, 2 }};

   score(t4);

   cout << "endsum10"<<endl;
   */findBest();
   
  int s;
  int m[N][N];
  int c=0;
  //      maybe all rows and all columns should have a count of 3 3 2?!
  /*  int tryC [8][8] = { {0,0,0,1,1,1,2,2},
		      {0,0,1,1,1,2,2,0},
		      {0,1,1,1,2,2,0,0},
		      {1,1,1,2,2,0,0,0},
		      {1,1,2,2,0,0,0,1},
		      {1,2,2,0,0,0,1,1},
		      {2,2,0,0,0,1,1,1},
		      {2,0,0,0,1,1,1,2}};
  
  cout << "Scory try " << score(tryC) <<endl; //48

  int trytri [8][8] = { {0,0,0,0,0,0,0,0},
			{0,0,0,0,0,0,0,1},
			{0,0,0,0,0,0,1,2},
			{0,0,0,0,0,1,2,1},
			{0,0,0,0,1,2,1,2},
			{0,0,0,1,2,1,2,1},
			{0,0,1,2,1,2,1,2},
			{0,1,2,1,2,1,2,1}};

  cout << "Score triangular " << score(trytri) <<endl; // 42


  int trytri2 [8][8] = { {1,2,1,2,1,2,1,0},
			 {2,1,2,1,2,1,0,1},
			 {1,2,1,2,1,0,1,2},
			 {2,1,2,1,0,1,2,1},
			 {1,2,1,0,1,2,1,2},
			 {2,1,0,1,2,1,2,1},
			 {1,0,1,2,1,2,1,2},
			 {0,1,2,1,2,1,2,1}};

  cout << "Score triangular " << score(trytri2) <<endl; // 48
  */
  cout << argc << endl;
  //  cout << argv[1] << endl;
  if (argc > 1 && !strcmp(argv[1],"Sim"))
    {
      do {
      cout << "Starting Simulated Annealing "<< c << endl;
      c++;
    // Create a random 8x8 matrix
      for (int i =0; i < 8 ; i++)
	for (int j =0; j < 8 ; j++)
	  m[i][j] = rand() % 3;
    
      simulatedAnnealing(m, initialTemperature);
      s = score(m);
      cout << s << endl;
      if (s == 37)
	{
	  cout << "Running Greedy"<< endl;
	  greedy(m);
	}
      s = score(m);
      } while(s >=37);
      printm(m);
    }
  else
    do {

cout << "Starting Greedy "<< c << endl;
      c++;
    // Create a random 8x8 matrix
      for (int i =0; i < 8 ; i++)
	for (int j =0; j < 8 ; j++)
	  m[i][j] = rand() % 3;
    
      greedy(m);
      s = score(m);
      cout << s << endl;
    } while(s >=37);
  printm(m);
  
}
