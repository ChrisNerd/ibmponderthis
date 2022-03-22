#include <iostream>
using namespace std;
int main()
{

  long int n = 2;
  const long int numSecsInWeek = 60*60*24*7;
  unsigned int as1[3]={0, 1, 0};
  unsigned int as2[3]={0, 1, 0};
  while (true)
    {
      as1[n%3] = (34*as1[(n-1)%3] - as1[(n-2)%3] + 2 + 1000)%1000;
      as2[n%3] = (34*as2[(n-1)%3] - as2[(n-2)%3] + 2 + numSecsInWeek)%numSecsInWeek;
      if (as1[n%3]==576)
	{
	  cout << "n" << n << "\nas1 " << as1[n%3] << "\nas2 " << as2[n%3] << endl;
	}
      n++;
    }
}
