// August 2011 IBM Ponder This
// Chris Shannon
// Calgary Canada
#include <iostream>
#include <stdlib.h>
#include <vector>
using namespace std;
double Herons(int, int, int);

class Triangle
{

 public:
  Triangle(int a, int b, int c, int perimeter, double area)
  :
  a_(a),
  b_(b),
  c_(c),
  perimeter_(perimeter),
  area_(area)
  {
  }

  bool isPerimeter(int perimeter)
  {
    return perimeter == perimeter_;
  }
  bool isArea(double area)
  {
    return area == area_;
  }

  int a()
  {
    return a_;
  }
  int b()
  {
    return b_;
  }
  int c()
  {
    return c_;
  }
  int perimeter()
  {
    return perimeter_;
  }
  double area()
  {
    return area_;
  }

 private:
  int a_;
  int b_;
  int c_;
  int perimeter_;
  double area_;
};

int MAXSIDELENGTH=100;

int main()
{
  vector<Triangle> tris;
  for (int a = 0; a <=MAXSIDELENGTH; a++)
    {
      for( int b=a; b <= MAXSIDELENGTH; b++)
	{
	  for (int c=b; c <= min(MAXSIDELENGTH, a+b); c++)
	    {
	      int perimeter = a+b+c;
	      double area = Herons(a,b,c);
	      cout.precision(15);
	      //  cout << a << " " << b << " " << c << " perimeter " << perimeter << " area " << area << endl;
	      Triangle t(a, b, c, perimeter, area);
	      tris.push_back(t);

	    } 
	}
    }
  // Now search for the solution
  // Note this can be sped way up by creating two new vectors
  // one sorted by perimeter and the other sorted by area
  // they can be searched in log(n) time with lower_bound and upper_bound
  for (int i = 0; i < tris.size(); i++)
    {
      int perimeterCan = tris[i].perimeter();
      double areaCan = tris[i].area();
      if (areaCan == 0)
	{
	  continue;
	}
      // is the area unique
      bool areaUnique = true;
      for(int j = 0; j < tris.size(); j++)
	{
	  if (i == j)
	    {
	      continue;
	    }
	  if(tris[j].isArea(areaCan))
	    {
	      areaUnique = false;
	      break;
	    }
	}
      if (areaUnique == true)
	{
	  continue;
	}
      vector<int> trisThatMatchPer;
      for(int j = 0; j < tris.size(); j++)
	{
	  if(tris[j].isPerimeter(perimeterCan))
	    {
	      trisThatMatchPer.push_back(j);
	    }
	}
      int numberOfTrisThatHaveUniqueArea = 0;
      for(int j = 0; j < trisThatMatchPer.size(); j++)
	{
	  double areaCan = tris[trisThatMatchPer[j]].area();
	  int waysToGetThisArea = 0;
	  for (int k = 0; k < tris.size(); k++)
	    {
	      if(tris[k].isArea(areaCan))
		{
		  waysToGetThisArea++;
		  if (waysToGetThisArea > 1)
		    {
		      break;
		    }
		}
	    }
	  if(waysToGetThisArea == 1)
	    {
	      numberOfTrisThatHaveUniqueArea++;
	    }
	}
      if (numberOfTrisThatHaveUniqueArea == trisThatMatchPer.size() - 1)
	{
	  //Solution Found!!
	  cout << "Candidate solution " << tris[i].a() << " " << tris[i].b() << " " << tris[i].c() << " perimeter " << tris[i].perimeter() << " area " << tris[i].area() << endl;

	}
    }
  
      
  return 0;
}

// Returns the area squared given the side lengths
double Herons(int a, int b, int c)
{
  double s =(1.0*a+b+c)/2;
  return s*(s-a)*(s-b)*(s-c);
}



/*
Output:
Candidate solution 1 4 4 perimeter 9 area 3.9375
Candidate solution 2 2 3 perimeter 7 area 3.9375
Candidate solution 3 4 4 perimeter 11 area 30.9375

Set that Charlie can form
1 4 4 perimeter 9 area 3.9375
2 3 4 perimeter 9 area 8.4375
3 3 3 perimeter 9 area 15.1875


Area=3.9375
1 4 4 perimeter 9 area 3.9375
2 2 3 perimeter 7 area 3.9375

unique for area
2 3 4 perimeter 9 area 8.4375

Unique for area
3 3 3 perimeter 9 area 15.1875



So now Ariella knows its either 1 4 4, or 2 2 3, perimeter 9 or 7.

If it 7 then Ariella thinks Charlie can form 1 3 3 area 2.1875 or 2 2 3 area 3.9375


unique for Area
1 3 3 perimeter 7 area 2.1875

1 4 4 doesn't work!!!  Given the area 3.9375 Arellia can't determine the perimeter.



Totally!!!!
3 4 4 perimeter 11 area 30.9375


1 5 5 perimeter 11 area 6.1875
2 4 5 perimeter 11 area 14.4375
3 3 5 perimeter 11 area 17.1875
3 4 4 perimeter 11 area 30.9375

grep "area 6\.1875$" tris.txt 
1 5 5 perimeter 11 area 6.1875
grep "area 14\.4375$" tris.txt 
2 4 5 perimeter 11 area 14.4375
grep "area 17\.1875$" tris.txt 
3 3 5 perimeter 11 area 17.1875
grep "area 30\.9375$" tris.txt 
2 6 7 perimeter 15 area 30.9375
3 4 4 perimeter 11 area 30.9375


grep "perimeter 15 " tris.txt 
1 7 7 perimeter 15 area 12.1875
2 6 7 perimeter 15 area 30.9375
3 5 7 perimeter 15 area 42.1875
3 6 6 perimeter 15 area 75.9375
4 4 7 perimeter 15 area 45.9375
4 5 6 perimeter 15 area 98.4375
5 5 5 perimeter 15 area 117.1875


Ariella thinks that if Charlie had a perimeter=15 there are still 2 possibilities for areas that can be form more than 1 way, i.e., area 42.1875 and 98.4375, and therefore wouldn't know the triangle.

grep "area 12\.1875$" tris.txt 
1 7 7 perimeter 15 area 12.1875
grep "area 42\.1875$" tris.txt 
1 13 13 perimeter 27 area 42.1875
3 5 7 perimeter 15 area 42.1875
grep "area 75\.9375$" tris.txt 
3 6 6 perimeter 15 area 75.9375
grep "area 45\.9375$" tris.txt 
4 4 7 perimeter 15 area 45.9375
grep "area 98\.4375$" tris.txt 
2 11 12 perimeter 25 area 98.4375
3 8 10 perimeter 21 area 98.4375
4 5 6 perimeter 15 area 98.4375
grep "area 117\.1875$" tris.txt 
5 5 5 perimeter 15 area 117.1875
*/

