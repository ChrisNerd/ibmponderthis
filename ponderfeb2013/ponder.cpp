#include <iostream>
#include <cmath>
using namespace std;
int main()
{
  int a=9999;
  for (int k=1; k<=10000;k++)
    {
      for (int i=1; i<=1000;i++)
	{
	  int l1=floor(log10(i))+1;
	  int l3=floor(log10(k))+1;
	  long int c=k*10000+a;
	  long int b=c*pow(10,l1)+i;
	  int l2=floor(log10(b*b))+1;
	  for (int j=1; j <=l2;j++)
	    {
	      long int secondhalf=(b*b)%((int)(pow(10,j)));
	      long int firsthalf=(b*b-secondhalf)/pow(10,j);
	      double firsthalfr=floor(sqrt(firsthalf));
	      double secondhalfr=floor(sqrt(secondhalf));
	      if (secondhalfr*secondhalfr==secondhalf && firsthalfr*firsthalfr==firsthalf && secondhalf>=pow(10,(j-1)))
		{
		  cout << firsthalf << " " << secondhalf << " " <<  b*b << " " << b <<endl;
		}
	    }
	}
    }
}
