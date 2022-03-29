#include <iostream>
#include <math.h>
#include <vector>
using namespace std;
bool conditionA(long int n);
bool conditionB(long int n);
bool conditionC(long int n);
bool isValid(long int n);
long int convertToInt(vector<int> n);
vector<int> nAsVector(long int n);
void recurs(vector<int>& n, int digit, const vector<int>& validSet);

int main()
{
  vector<int> validSet;
  validSet.push_back(0);
  validSet.push_back(1);
  validSet.push_back(2);
  validSet.push_back(5);
  validSet.push_back(6);
  validSet.push_back(8);
  validSet.push_back(9);

  for (int numDigits = 1; numDigits <=6; numDigits++)
    {
      vector<int> n(numDigits);
      for (int digit = numDigits - 1; digit >= 0 ; digit--)
	{
	  recurs(n,digit,validSet);	  
	}
    }
}

void recurs(vector<int>& n, int digit, const vector<int>& validSet)
{
  if (digit < 0)
    {
      // This is the n we have to check
      long int nInt = convertToInt(n);
      if (isValid(nInt*nInt))
	{
	  cout << nInt << endl;
	}
      return;
    }
  
  for (int k = 0; k < validSet.size();k++)
    {
      n[digit] = validSet[k];
      recurs(n,digit-1, validSet);
    }
}



long int convertToInt(vector<int> n)
{
  long int result = 0;
  for (int i = 0; i < n.size(); i++)
    {
      result += n[i] * pow(10,n.size()-i-1);
    }
  return result;
}

bool isValid(long int n)
{
  n = n%1000000;
  long int numInts = log10(n);
  while (numInts >= 0)
    {
      int k =n/pow(10,numInts);
      if (k == 3 || k==4 || k==7)
	{
	  return false;
	}
      n -= k * pow(10,numInts);
      numInts--;
    }
  return true;
}
