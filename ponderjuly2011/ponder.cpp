#include <iostream>
#include <stdlib.h>
#include <time.h>
using namespace std;
int checkAll(int* arr);
bool check123(int a, int b, int c, int* arr);
void printArr(int* arr);
void createPopulationFromScratch(int arrayOfArrays[][14*80]);
int createPopulationFromTop5(int arrayOfArrays[][14*80]);
int POPULATIONSIZE=105;

int main()
{
  int arrayOfArrays[POPULATIONSIZE][14*80];
  /* initialize random seed: */
  srand ( time(NULL) );

  createPopulationFromScratch(arrayOfArrays);

  for(int i = 0; i < 2500; i++)
    {
      int score = createPopulationFromTop5(arrayOfArrays);
      cout << "Population " << i << " score " << score << endl;
      if (i%100 ==0)
	{
	  printArr(arrayOfArrays[100]);
	}

      if (score == 82160)
	{
	  cout << "done " << endl;
	  break;
	}
    }

  int score = checkAll(arrayOfArrays[100]);
  cout << score<< endl;
  printArr(arrayOfArrays[100]);
  
  return 0;
}

void createPopulationFromScratch(int arrayOfArrays[][14*80])
{
  for(int popi = 0; popi < POPULATIONSIZE; popi++)
    {
      int* arr = arrayOfArrays[popi];
      // Initialize the first day
      for(int i = 0; i < 80; i++)
	{
	  arr[i] = i%3;
	}
      // The rest of the array is random
      for(int i=80; i<14*80;i++)
	{
	  arr[i] = rand()%3;
	}
    }
}

int createPopulationFromTop5(int arrayOfArrays[][14*80])
{
  int scores[POPULATIONSIZE];
  for (int i = 0; i <POPULATIONSIZE; i++)
    {
      scores[i] = checkAll(arrayOfArrays[i]);
    }

  int top5[5];
  // Find top
  top5[0] =0;
  int topScore = scores[top5[0]];
  for (int i = 0; i < POPULATIONSIZE; i++)
    {
      if (scores[i] > topScore)
	{
	  topScore = scores[i];
	  top5[0] = i;
	}
    }

  // Find second place
  top5[1]=0;
  topScore = scores[top5[1]];
  for (int i = 0; i < POPULATIONSIZE; i++)
    {
      if (scores[i] > topScore && i != top5[0])
	{
	  topScore = scores[i];
	  top5[1] = i;
	}
    }

  // Find third place
  top5[2]=0;
  topScore = scores[top5[2]];
  for (int i = 0; i < POPULATIONSIZE; i++)
    {
      if (scores[i] > topScore && i != top5[0]
	  && i != top5[1])
	{
	  topScore = scores[i];
	  top5[2] = i;
	}
    }

  // Find fourth place
  top5[3]=0;
  topScore = scores[top5[3]];
  for (int i = 0; i < POPULATIONSIZE; i++)
    {
      if (scores[i] > topScore && i != top5[0]
	  && i != top5[1]
	  && i != top5[2])
	{
	  topScore = scores[i];
	  top5[3] = i;
	}
    }


  // Find fifth place
  top5[4]=0;
  topScore = scores[top5[4]];
  for (int i = 0; i < POPULATIONSIZE; i++)
    {
      if (scores[i] > topScore && i != top5[0]
	  && i != top5[1]
	  && i != top5[2]
	  && i != top5[3])
	{
	  topScore = scores[i];
	  top5[4] = i;
	}
    }


  // Now for all 5C2=10, we'll make 10 offspring for each pair, so there will be a new population of 100.  We'll add the original top 5 to make 105.

  // Copy over the top 5
  // copy them to a 5x80 temp array first to avoid accidentally overwriting
  // the top five will take place from 100-104.
  int tempArrayOfArrays[5][14*80];
  for (int i = 0; i < 5; i++)
    {
      for (int j = 0; j < 14*80; j++)
	{
	  tempArrayOfArrays[i][j] = arrayOfArrays[top5[i]][j];
	}
    }
  for (int i = 0; i < 5; i++)
    {
      for (int j = 0; j < 14*80; j++)
	{
	  arrayOfArrays[100+i][j] = tempArrayOfArrays[i][j];
	}
    }

  int populationID = 0;
  for (int top5a = 100; top5a < 104; top5a++)
    {
      for (int top5b = top5a+1; top5b < 105; top5b++)
	{
	  for(int childCount =0; childCount < 10; childCount++)
	    {
	      // create population from A and B
	      int arr[80*14];
	      int* arrA = arrayOfArrays[top5a];
	      int* arrB = arrayOfArrays[top5b];

	      for(int j= 0; j < 80*14; j++)
		{
		  if(rand()%1000 < 999)
		    {
		      // If the parents are the same, pass it to the child
		      if(arrA[j] == arrB[j])
			{
			  arr[j] = arrA[j];
			}
		      else
			// if the parents differ, choose 1 or the other with 50-50
			{
			  if(rand() %2==0)
			    {
			      arr[j] = arrA[j];
			    }
			  else
			    {
			      arr[j] = arrB[j];
			    }
			}
		    }
		  else
		    {
		      // 1% odds it will get random mutation
		      arr[j] = rand()%3;
		    }
		 
		  // Copy the new array
		  for (int i = 0; i < 14*80; i++)
		    {
		      arrayOfArrays[populationID][i] = arr[i];
		    }
		}
		  populationID++;
	    }

	}
    }
  return scores[top5[0]];
}

// Check
int checkAll(int* arr)
{
  int numCorrect=0;
  // 80 c 3 = 82160
  for(int a = 0; a <=77; a++)
    {
      for(int b = a+1; b <=78; b++)
	{
	  for(int c = b+1; c<=79; c++)
	    {
	      if (check123(a,b,c,arr))
		numCorrect++;
	    }
	}
    }
  return numCorrect;
}



bool check123(int a, int b, int c, int* arr)
{
  for(int day =0; day < 14; day++)
    {
      if (arr[80*day + a] != arr[80*day + b]
	  && arr[80*day + a] != arr[80*day + c]
	  && arr[80*day + b] != arr[80*day + c])
	{
	  return true;
	}
    }
  return false;
}

void printArr(int* arr)
{
  for (int day=0; day < 14; day++)
    {
      for(int i =0; i< 80; i++)
	{
	  cout << arr[80*day + i];
		      if (i !=79)
			cout << " ";
	}
      cout << endl;
    }
}
