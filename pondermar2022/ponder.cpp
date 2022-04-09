#include <iostream>
#include <vector>
#include <math.h>
#include <climits>
#include <iomanip>
#include <unordered_map>
#include <algorithm>    // std::sort
#include <execution>

using namespace std;
// g++ ponder.cpp -ltbb && time ./a.out 7

bool isPrime(int n)
{
  for(int i=2; i * i <= n; i++)
    if (n % i == 0)
      return false;
  return true;
}

bool isPrimeWithPrimerList(int n, const vector<int>& primeList)
{
  for(int i=0; primeList[i]*primeList[i] <= n; i++)
    if (n% primeList[i] == 0)
      return false;
  return true;
}

int patternToInt(const string& p, const string& q, const vector<int>& powersOf3)
{
  int r=0;
  for(int i=0; i<p.length(); i++)
    {
      if (p[i] == q[i])
	r += 2*powersOf3[i];
      else if( q.find(p[i]) != std::string::npos)
	r += 1*powersOf3[i];

    }
  return r;
}


int countDistinct(const string& s) 
{ 
  unordered_map<char, int> m; 
  for (int i = 0; i < s.length(); i++)
    m[s[i]]++; 
  return m.size(); 
} 

bool sortingFunction(const string& p, const string& q)
{
  return (countDistinct(p) > countDistinct(q));
}

int main(int argc, char *argv[])
{
  vector<string> allnDigitPrimes;
  int numDigits = atoi(argv[1]);
  vector<int> powersOf3;
  for(int i=0; i < numDigits; i++)
    powersOf3.push_back( pow(3,i));
  vector<int> allPrimes;
  allPrimes.push_back(2);
  cout << "primes start " << endl;
  for (int i=3; i < pow(10,numDigits); i++)
    if (isPrimeWithPrimerList(i, allPrimes))
      allPrimes.push_back(i);

  cout << allPrimes.size() << " primes found." << endl;

  for(int i=0; i < allPrimes.size(); i++)
    if (allPrimes[i] >= pow(10,numDigits-1))
      allnDigitPrimes.push_back(to_string(allPrimes[i]));

  /*  for (int i=pow(10,numDigits-1); i < pow(10,numDigits); i++)
      if (isPrime(i))
      allnDigitPrimes.push_back(to_string(i));*/

  cout << "here 1" <<endl;
    sort (std::execution::par, allnDigitPrimes.begin(), allnDigitPrimes.end(), sortingFunction);
  cout << "here 2" <<endl;

    vector<string> allnDigitPrimesReversed = allnDigitPrimes;
  cout << "here 3" <<endl;
    reverse(allnDigitPrimesReversed.begin(), allnDigitPrimesReversed.end());
  cout << "here 4" <<endl;

  cout << "Start of primes " << allnDigitPrimes[0] << endl;
  cout << "Start of primes reversed " << allnDigitPrimesReversed[0] << endl;
  
  cout << "There are " << allnDigitPrimes.size() << " primes with " << numDigits << " digits." << endl;

  cout << "Pattern 12345, 15623 " << patternToInt("12345","15623", powersOf3) << " should be 95." << endl;

  vector<string> primesFiltered;
  for(int i =0; i < allnDigitPrimes.size(); i++)
    {
      string pi=allnDigitPrimes[i];
      if (pi[0] == '1' && countDistinct(pi) == numDigits)
	primesFiltered.push_back(pi);
    }
  cout << "primesFiltered[0] " << primesFiltered[0] <<  " primesFiltered.size " << primesFiltered.size() << endl;
  long long unsigned int minSoFar = ULLONG_MAX;
  string minElementSoFar;//=0; // should be -1 but results in a seg fault at first printing
  int counter=0;
  //  for(int pi=0; pi < allnDigitPrimes.size(); pi++)

  /*
    int a[] = {0,1};
    std::vector<int> v;
    std::for_each(std::execution::par, std::begin(a), std::end(a), [&](int i) {
     v.push_back(i*2+1); // Error: data race
    });
  */
  //  for_each(std::execution::par, begin(allnDigitPrimes), end(allnDigitPrimes), [&](string pi)
  for_each(std::execution::par, begin(primesFiltered), end(primesFiltered), [&](string pi)
    {
      vector<long int> buckets(pow(3,numDigits), 0);
      long long unsigned int runningSum = 0;
      if (counter % 1000 == 0)
      cout << "counter " << counter << " pi " << pi << /*" prime " << allnDigitPrimes[pi] <<*/ " minSoFar " << minSoFar << " minElementSoFar " << minElementSoFar << /*" minPrimeSoFar " << allnDigitPrimes[minElementSoFar] << */endl;
	  for(int qi=0; qi < allnDigitPrimesReversed.size(); qi++)
	{
	  int patternNumber = patternToInt(pi, allnDigitPrimes[qi], powersOf3);
	   int oldBucket = buckets[patternNumber];
	  buckets[patternNumber]++;
	    runningSum += 2*oldBucket + 1;

	  /*
	  cout << pi << " "<< allnDigitPrimes[pi] << endl;
	  cout << qi << " "<< allnDigitPrimes[qi] << endl;
	  cout << "patternNumber " << patternNumber << endl;
	  cout << "oldBucket " << oldBucket << endl;
	  cout << "buckets[patternNumber] " << buckets[patternNumber] << endl;
	  cout << "runningSum" << runningSum << endl;
	  */
	  
	  if (runningSum > minSoFar)
	   break;
	}
      /*long temp;
            for (int i =0; i< buckets.size(); i++)
	{
	  temp = buckets[i];
	  runningSum += temp * temp;
	  }*/
      if (runningSum < minSoFar)
	{
	  minSoFar = runningSum;
	  minElementSoFar = pi;
	}
      counter++;
    });
    
  cout << "Done. minSoFar " << minSoFar << " minSoFar/size " << setprecision(18) << minSoFar*1.0/allnDigitPrimes.size() << " . minElementSoFarIndex " << minElementSoFar << /*" minElementSoFar " << allnDigitPrimes[minElementSoFar] << */ endl;
  cout << "Done with numDigits " << numDigits <<endl;
  return 0;
}
