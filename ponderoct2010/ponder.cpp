// Chris Shannon - Calgary Canada
// October 2010 IBM Ponder This

#include <iostream>
#include <math.h>
void CheckAll12EdgeOrderings(int, int, int, int, int);
bool CheckAll10RightAnglePlacements(int, int, int, int, int, bool& , double& );
bool CalculateAreaWithAdjacentRightAngles(int,int,int,int,int,double&);
bool CalculateAreaWithOppositeRightAngles(int,int,int,int,int,double&);
double Herons(double, double, double);

using namespace std;
int MAX_LENGTH=66;
double MAX_TOLERANCE=0.000001;
int main()
{
  for (int a = 1; a <= MAX_LENGTH; a++)
    {
      cout << "a = " << a << endl;
      for (int b = a+1; b <= MAX_LENGTH; b++)
	{
	  for (int c = b+1; c <= MAX_LENGTH; c++)
	    {

	      for (int d = c+1; d <= MAX_LENGTH; d++)
		{
		  for (int e = d+1; e <= MAX_LENGTH; e++)
		    {
		      // There are 12 ways of arranging the 5 sides in a ring,
		      // and 10 combinations of setting the 2 right angles
		      // so there are 120 possible pentagon.
		      // Some of the 120 may be invalid
		      // All valid pentagons must produce the same area

		      CheckAll12EdgeOrderings(a,b,c,d,e);	
		    }
		}
	    }
	}
    }
}

void CheckAll12EdgeOrderings(int a, int b, int c, int d, int e)
{
  bool valid = true;
  bool foundFirst = false;
  double firstArea=0.0;
  valid = valid && CheckAll10RightAnglePlacements(a,b,c,d,e, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,b,c,e,d, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,b,d,c,e, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,b,d,e,c, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,b,e,c,d, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,b,e,d,c, foundFirst, firstArea);

  valid = valid && CheckAll10RightAnglePlacements(a,c,b,d,e, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,c,b,e,d, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,c,d,b,e, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,c,e,b,d, foundFirst, firstArea);

  valid = valid && CheckAll10RightAnglePlacements(a,d,b,c,e, foundFirst, firstArea);
  valid = valid && CheckAll10RightAnglePlacements(a,d,c,b,e, foundFirst, firstArea);

  if (valid && foundFirst)
    {
      cout << "a " << a << " b " << b << " c " << c <<  " d " << d <<  " e " << e << " area " << firstArea<< endl; 
    }
}


bool CheckAll10RightAnglePlacements(int a, int b, int c, int d, int e, bool& foundFirst, double& firstArea)
{
  double area=0.0;
  bool validPentagon = false;
  validPentagon = CalculateAreaWithAdjacentRightAngles(a,b,c,d,e,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }

  validPentagon = CalculateAreaWithAdjacentRightAngles(b,c,d,e,a,area);
  if (validPentagon) 
    {
        if (foundFirst)
        {
            if (fabs(firstArea - area) > MAX_TOLERANCE)
            {
                return false;
            }
        }
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithAdjacentRightAngles(c,d,e,a,b,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithAdjacentRightAngles(d,e,a,b,c,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithAdjacentRightAngles(e,a,b,c,d,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }

  validPentagon = CalculateAreaWithOppositeRightAngles(a,b,c,d,e,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithOppositeRightAngles(b,c,d,e,a,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithOppositeRightAngles(c,d,e,a,b,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithOppositeRightAngles(d,e,a,b,c,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  validPentagon = CalculateAreaWithOppositeRightAngles(e,a,b,c,d,area);
  if (validPentagon) 
    {
      if(foundFirst)
	{
	  if (fabs(firstArea - area) > MAX_TOLERANCE)
	    {
	      return false;
	    }
	}
      else
	{
	  foundFirst = true;
	  firstArea = area;
	}
    }
  return true;
}

bool CalculateAreaWithAdjacentRightAngles(int a, int b, int c, int d, int e, double& area)

{
  //    cout << "adjacent"<< "a " << a << " b " << b << " c " << c <<  " d " << d <<  " e " << e << endl;
  int f2 = a*a + (e-b)*(e-b);
  double f = sqrt(f2);

  if (f > c+d
      || d > f+c
      || c > f+d)
    {
      return false;
    }
  double angleA1;
  double angleC;
  double angleA2;
  double angleD;

  if(e>b)
    {
      angleA1 = atan(static_cast<double>(a)/(e-b));
      angleC = acos(static_cast<double>(f2+d*d-c*c)/(2*f*d));
      if (angleA1 + angleC + MAX_TOLERANCE >= M_PI)
	{
	  return false;
	}

      angleA2 = atan(static_cast<double>(e-b)/a);
      angleD = acos(static_cast<double>(f2+c*c-d*d)/(2*f*c));
      if (angleA2 + angleD + MAX_TOLERANCE >= M_PI /2)
	{
	  return false;
	}
    }
  else
    {
      angleA1 = atan(static_cast<double>(a)/(b-e));
      angleD = acos(static_cast<double>(f2+c*c-d*d)/(2*f*c));
      if (angleA1 + angleD + MAX_TOLERANCE >= M_PI)
	{
	  return false;
	}
      angleA2 = atan(static_cast<double>(b-e)/a);
      angleC = acos(static_cast<double>(f2+d*d-c*c)/(2*f*d));

      if (angleA2 + angleC + MAX_TOLERANCE >= M_PI /2)
	{
	  return false;
	}
    }

  if (angleA1 < MAX_TOLERANCE || 
      angleA2 < MAX_TOLERANCE || 
      angleC < MAX_TOLERANCE || 
      angleD < MAX_TOLERANCE)
    {
      return false;
    }

  area = a*(e+b)/2.0 + Herons(f2,c*c,d*d);
  return true;

}
bool CalculateAreaWithOppositeRightAngles(int a, int b, int c, int d, int e, double& area)
{
  //    cout << "opposite"<< "a " << a << " b " << b << " c " << c <<  " d " << d <<  " e " << e << endl;
  int f2 = a*a + e*e;
  int g2 = b*b + c*c;
  double f = sqrt(f2);
  double g = sqrt(g2);
  if (f > g+d ||
      g > f+d ||
      d > f+g)
    {
      return false;
    }

  double angleAE = atan(static_cast<double>(e)/a);
  double angleBC = atan(static_cast<double>(c)/b);
  double angleD = acos(static_cast<double>(f2+g2-d*d)/(2*f*g));
  if (angleAE + angleBC + angleD + MAX_TOLERANCE >= M_PI)
    {
      return false;
    }

  double angleCB = atan(static_cast<double>(b)/c);
  double angleF = acos(static_cast<double>(g2 + d*d -f2)/(2*g*d));
  if (angleCB + angleF + MAX_TOLERANCE >= M_PI)
    {
      return false;
    }

  double angleEA = atan(static_cast<double>(a)/e);
  double angleG = acos(static_cast<double>(f2 +d*d -g2)/(2*f*d));
  if (angleEA + angleG + MAX_TOLERANCE >= M_PI)
    {
      return false;
    }
  

  if(angleAE < MAX_TOLERANCE ||
     angleBC < MAX_TOLERANCE || 
     angleD < MAX_TOLERANCE ||
     angleCB < MAX_TOLERANCE ||
     angleF < MAX_TOLERANCE ||
     angleEA < MAX_TOLERANCE ||
     angleG< MAX_TOLERANCE )
    {
      return false;
    } 
  
  area = a*e/2.0 +b*c/2.0 + Herons(f2, g2, d*d);
  return true;
}


double Herons(double a2, double b2, double c2)
{
  return 1/4.0 * sqrt((a2 + b2 +c2)*(a2 + b2 +c2) - 2 * (a2*a2 +b2*b2+c2*c2));
}

