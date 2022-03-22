/* Run g++ main.cpp && ./a.out  (and wait a while, and ensure you have at least 6 GB of memory) to solve November 2012 IBM Ponder This
 * Chris Shannon
 * Calgary Canada
 */


#include <iostream>
#include <limits>
#include <iomanip>
#include <algorithm>
#include <list>
#include <queue>
#include <math.h>
using namespace std;
unsigned long R =9801;


/*
With Newton's method

* solution ij = 6053 956 and 3628 573
Maxdiff = 0.000102041 between ij = 1 0 and 9800 1
Maxr = 0.0001020304045296295 between ij = 1 0 and 9800 1
0.0001020304055917867
numGaps = 22932501
numGaps 10 medge1 8489 1312 medge2 1197 185 r 0.0001020304029352785
numGaps 9 medge1 4901 1 medge2 4900 1 r 0.0001020304029363949
numGaps 8 medge1 4891 4289 medge2 2478 2173 r 0.0001020304029365397
numGaps 7 medge1 121 9 medge2 9653 718 r 0.0001020304029365903
numGaps 6 medge1 7374 149 medge2 2425 49 r 0.000102030402939872
numGaps 5 medge1 8018 1877 medge2 1525 357 r 0.0001020304029400119
numGaps 4 medge1 4927 2757 medge2 3626 2029 r 0.0001020304029446391
numGaps 3 medge1 2798 1493 medge2 5849 3121 r 0.0001020304029545462
numGaps 2 medge1 6923 4381 medge2 1359 860 r 0.0001020304029646963
numGaps 1 medge1 1 0 medge2 9800 1 r 0.0001020304045296295        r 0.0001020304045296295 -wolf x =  0.0001020304045296294523225735    x = 1/sqrt(96059602)
numGaps 0 medge1 6053 956 medge2 3628 573 r 0.000102030404562728  r 0.000102030404562728 - wolf x ~~ 0.000102030404529629452322573494..x = 1/sqrt(96059602)
*** Exited normally ***
*/

unsigned long squaresize=(R+1)*(R+1);


/*
 * Edge class represents the left most point and the right most point of a tree  
 * It keeps track of whether it is blocked by another tree (visible)
 * and if it forms part of a background gap
 */
class Edge
{
public:
  Edge(int ini, int inj, bool inleft)
  {
    background = true;
    visible = true;
    i = ini;
    j = inj;
    left = inleft;
    r2 = i*i+j*j;
    r = sqrt(r2);
  }
  
public:
  // private:
  int i;
  int j;
  unsigned long r2;
  double r;
  bool left; // true for left, false for right
  bool background;
  bool visible;
  Edge* next;
  Edge* prev;
};


/* Centre of a tree
 */
class Point
{
public:
  Point(int ini, int inj)
  {
    i=ini;
    j=inj;
    theta = atan2(j,i);
  }
  
public:
  int i;
  int j;
  double theta;
};

/* When a pair of tree edges get to a size r, they close their gap.  This class calculates that r value for two edges.
 */

class Collision
{
public:
  Collision(){}
  Collision(Edge* edge1, Edge* edge2)
  {
    medge1 = edge1;
    medge2 = edge2;
    r = CalculateR(); 
    
  }
public:
  double CalculateR()
  {
    // there are invalid cases
    
    double theta1 = atan2(medge1->j,medge1->i);
    double theta2 = atan2(medge2->j,medge2->i);
    
    if (theta1 > theta2)
    {
      swap(medge1, medge2);
      swap(theta1, theta2);
    }
    // Now medge1 is to the right of medge2
    
    
    if (medge2->left && !medge1->left)
    {
      cout << "error, no collision possible" <<endl;
      return 1;
    }
    
    if (medge1->left && medge2->left && medge1->r2 > medge2->r2)
    {
      cout << "error 2, no collision possible" <<endl;
      return 1;
    }
    if (!medge1->left && !medge2->left && medge1->r2 < medge2->r2)
    {
      cout << "error 3, no collision possible" <<endl;
      return 1;
    }
    int coef1=1;
    int coef2=1;
    /*  if (!medge1->left)
     *	coef1=-1;
     *	if (!medge2->left)
     *	coef2=-1;
     */
    // solve for r
    // coef1* arcsin (r/R1)+coef2 * arcsin(r/R2) = theta2-theta1;
    double R1 = medge1->r;
    double R2 = medge2->r;
    if (medge1->left != medge2->left)
    {
      // It remains a mystery when the error between the closed form solution and Newton's method stays very small ( <10^-12) for all case
      // the end result is vastly different.  The correct answer is found using Newton's method however.
      
      double nm = newtonsMethod(1,1, R1, R2, theta2-theta1);
      double ds = directSolve(medge1->i, medge1->j, medge2->i, medge2->j);
      double errordiff = nm -ds;
      if (fabs(errordiff) > pow(10,-12))
      {
	cout << std::setprecision (numeric_limits<double>::digits10 + 1) << "Newton's method " << nm << " Direct Solve " << ds << " error: " << errordiff << endl;
	cout << medge1->i << " " << medge1->j << " " << medge2->i << " " << medge2->j<<endl;
      }
      
      return newtonsMethod(1,1, R1, R2, theta2-theta1);
      //return directSolve(medge1->i, medge1->j, medge2->i, medge2->j);
    }
    else if (medge1->left == medge2->left && medge1->left == true)
	return newtonsMethod(1,-1, R1, R2, theta2-theta1);
      else if (medge1->left == medge2->left && medge1->left == false)
	return newtonsMethod(-1,1, R1, R2, theta2-theta1);
  }
  // private:
  double r;
  Edge* medge1;
  Edge* medge2;
  
  // Closed form solution to arcsin (r/R1)+ arcsin(r/R2) = theta2-theta1;
  double directSolve(int i1, int j1, int i2, int j2)
  {
    unsigned long e = i1*i1+j1*j1;
    unsigned long f = i2*i2+j2*j2;
    unsigned long g = i1*j2-j1*i2;
    unsigned long h = i1*i2+j1*j2;
    double i = 1.0*g/h;
    double j = sqrt(1+i*i);
    double k = sqrt(e*f);
    double l = k*i/j;
    double m = sqrt(l*l*(e + f-2*sqrt(e*f-l*l))/(e*e-2*e*f+f*f+4*l*l));
    return m;
    
    
  }
  
  double newtonsMethod(int coef1, int coef2, double R1, double R2, double dtheta)
  {
    
    //x = -(i sqrt(a) sqrt(b) (i sin(2 t)+cos(2 t)-1))/(sqrt(cos(t)+i sin(t)) sqrt((4 i b sin(t))/a+(4 i a sin(t))/b+(4 b cos(t))/a+(4 a cos(t))/b+4 i sin(2 t)+4 cos(2 t)+4))
    //x = a b sin(t) Re(1/sqrt(a^2+2 a b cos(t)+b^2))
    
    // wolf solve(arcsin(x/a)- arcsin(x/b) = t)
    //return R1*R2 * sin(dtheta) / sqrt(R1*R1 + 2*R1*R2 * cos(dtheta) + R2*R2);
    double rd=.0001;
    double r2;
    do 
    {
      r2=rd;
      rd = rd - (coef1 * asin(rd/R1) + coef2 * asin(rd/R2)-dtheta)/(coef1/sqrt(R1*R1-rd*rd) + coef2/sqrt(R2*R2-rd*rd));
    }
    while (fabs(r2-rd) > 1.0*pow(10,-10));
    if (rd > 1.1)
      cout << "small" <<endl;
    return rd;
  }
};

bool compare_theta (Point& first, Point& second)
{
  if (first.theta < second.theta) return true;
  else return false;
}

class compare_r
{
public:
  bool operator() (Collision& first, Collision& second)
  {
    return (first.r > second.r);
  }
};

bool removeFurtherEdge(Collision& c)
{
  if (c.medge1->r2 > c.medge2->r2)
  {
    c.medge1->visible = false;
    c.medge1->prev->next = c.medge1->next;
    c.medge1->next->prev = c.medge1->prev;
  }
  else if (c.medge1->r2 < c.medge2->r2)
  {
    c.medge2->visible = false;
    c.medge2->prev->next = c.medge2->next;
    c.medge2->next->prev = c.medge2->prev;
  }
  else
  {
    // remove both.  This occurs when a number is a sum of two squares two different ways
    c.medge1->visible = false;
    c.medge1->prev->next = c.medge1->next;
    c.medge1->next->prev = c.medge1->prev;
    c.medge2->visible = false;
    c.medge2->prev->next = c.medge2->next;
    c.medge2->next->prev = c.medge2->prev;
  }
}

int main()
{
  
  // Create points of interest using a method similar to the sieve of Eratosthenes.
  vector<bool> square(squaresize);
  for (unsigned long i =0; i< squaresize;i++)
  {
    square[i]=true;
  }
  for (unsigned long i =1; i <= R; i++)
  {
    for (unsigned long j=0; j <= i; j++)
    {
      if (i*i + j*j > R*R || square[(R+1)*j + i]==false)
	continue;
      unsigned long k = 2;
      while (k*k * (i*i+j*j) <= R * R)
      {
	square[k*j*(R+1)+k*i]=false;
	k++;
      }
    }
  }
  // Now we have visible points.  Put them in a linked list
  list<Point> pointlist;
  
  for (unsigned long i =1; i <=R; i++)
  {
    for (unsigned long j=0; j <= i; j++)
    {
      if (i*i + j*j > R*R || square[(R+1)*j + i]==false)
	continue;
      pointlist.push_back(Point(i,j));
    }
  }
  
  square.clear();
  pointlist.sort(compare_theta);
  cout << pointlist.front().i
  << " " << pointlist.front().j << "\n"
  << pointlist.back().i << " "  << pointlist.back().j <<
  "\n" << pointlist.size() <<endl;
  
  // Find max diff
  double maxdiff = 0;
  list<Point>::iterator it;
  list<Point>::iterator itmaxdiff;
  list<Point>::iterator secondlast = pointlist.end();
  secondlast--;
  
  double maxr = 0;
  Collision d;
  list<Point>::iterator itmaxr;
  
  for ( it=pointlist.begin() ; it != secondlast; it++ )
  {
    list<Point>::iterator itnext = it;
    itnext++;
    
    double diff = itnext->theta - it->theta;
    if (diff > maxdiff)
    {
      itmaxdiff = it;
      maxdiff = diff;
    }
    double r = d.directSolve(it->i, it->j, itnext->i, itnext->j);
    if (r > maxr)
    {
      itmaxr = it;
      maxr=r;
    }
    if (fabs(r -0.0001020304045296295) < pow(10,-13))
    {
      cout << "solution ij = " << it->i << " " << it->j << " and " << itnext->i << " " << itnext->j << endl;
    }
    
  }    
  cout << "Maxdiff = " << maxdiff << " between ij = " << itmaxdiff->i << " " << itmaxdiff->j ;
  itmaxdiff++;
  cout<<   " and " << itmaxdiff->i << " " << itmaxdiff->j << endl;
  std::cout << std::setprecision (numeric_limits<double>::digits10 + 1)  << "Maxr = " << maxr << " between ij = " << itmaxr->i << " " << itmaxr->j ;
  itmaxr++;
  cout<<   " and " << itmaxr->i << " " << itmaxr->j << endl;
  Collision c;
  double rstar = c.newtonsMethod(1,1,1.0, sqrt(1+9801*9801), maxdiff);
  
  std::cout << std::setprecision (numeric_limits<double>::digits10 + 1) << rstar <<endl;
  
  //  Maxdiff = 0.000102041 between ij = 1 0 and 9800 1
  //  Maxr = 0.0001020304045296295 between ij = 1 0 and 9800 1
  //  0.0001020304055917867
  
  secondlast = pointlist.end();
  secondlast--;
  priority_queue<Collision, vector<Collision>, compare_r> pq;
  Edge* edgeBegin;
  Edge* edgeEnd;
  
  // Initialize all the collisions between adjacent pairs of edges.
  for ( it=pointlist.begin() ; it != secondlast; it++ )
  {
    //   make a lefttravelingedge, mark it as background
    Edge* l = new Edge(it->i,it->j,true);
    
    list<Point>::iterator itnext = it;
    itnext++;
    //   make a righttravelingedge, mark it as background
    Edge* r = new Edge(itnext->i,itnext->j,false);
    
    if (it==pointlist.begin())
    {
      l->prev=NULL;
      l->next=r;
      r->prev=l;
      edgeBegin=l;
    }
    else
    {
      l->prev=edgeEnd;
      l->next=r;
      r->prev=l;
      l->prev->next = l;
    }
    edgeEnd=r;
    
    Collision c(l,r);
    pq.push(c);
  }
  edgeEnd->next=NULL;
  unsigned long numGaps = pq.size();
  cout << "numGaps = " << numGaps << endl;
  
  pointlist.clear();
  
  //run event driven simulation until the last gap is closed.
  while (numGaps>0)
  {
    // place the collision event in a heap if it occurs when R < RObvious
    Collision c = pq.top();
    pq.pop();  
    
    if (!c.medge1->visible || !c.medge2->visible)
      continue;
    
    
    if (c.medge1->left == c.medge2->left)
    {
      //  process leftleft
      //  process rightright
      // determine which is further
      // make nearer.background = further.background
      // remove further from linked list
      // add new collision
      if (c.medge1->r2 > c.medge2->r2)
      {
	bool inOrder = c.medge1->next == c.medge2;
	c.medge1->visible = false;
	if (c.medge1->prev)
	  c.medge1->prev->next = c.medge1->next;
	if (c.medge1->next)
	  c.medge1->next->prev = c.medge1->prev;
	
	if (inOrder)
	{
	  Edge* e1 = c.medge2->prev;
	  if(e1 && c.medge2)
	  {
	    Collision c2(e1,c.medge2);
	    pq.push(c2);
	  }
	}
	else
	{
	  Edge* e1 = c.medge2->next;
	  if(e1 && c.medge2)
	  {
	    Collision c2(e1,c.medge2);
	    pq.push(c2);
	  }
	}
      }
      else if (c.medge1->r2 < c.medge2->r2)
      {
	bool inOrder = c.medge1->next == c.medge2;
	c.medge2->visible = false;
	if (c.medge2->prev)
	  c.medge2->prev->next = c.medge2->next;
	if (c.medge2->next)
	  c.medge2->next->prev = c.medge2->prev;
	if (inOrder)
	{
	  Edge* e2 = c.medge1->next;
	  if (e2 && c.medge2)
	  {
	    Collision c2(e2,c.medge2);
	    pq.push(c2);
	  }
	}
	else
	{
	  Edge* e2 = c.medge2->prev;
	  if(e2 && c.medge2)
	  {
	    Collision c2(e2,c.medge2);
	    pq.push(c2);
	  }
	}
      }
    }
    else
    {
      // process leftright
      //  3 case, 
      //  0 are background - remove the further one
      //  1 is background - should not be possible throw error
      //  both are background - reduce gap count by 1, remove the further one, update linked list, add a new collision event to the heap.
      if (!c.medge1->background && !c.medge2->background)	
      { 
	removeFurtherEdge(c);
      }
      else if (c.medge1->background && c.medge2->background)
      {
	numGaps--;	      
	if (numGaps <= 10)
	{
	  cout << "numGaps " << numGaps << " medge1 " << c.medge1->i << " " << c.medge1->j << " medge2 "  << c.medge2->i << " " << c.medge2->j << " r " << setprecision(numeric_limits<double>::digits10 + 1)<< c.r << endl;
	}
	unsigned long e1r2 = c.medge1->r2;
	unsigned long  e2r2 = c.medge2->r2;
	if ( e1r2 > e2r2 )
	{
	  bool inOrder = c.medge1->next == c.medge2;
	  c.medge1->visible = false;
	  if (c.medge1->prev)
	    c.medge1->prev->next = c.medge1->next;
	  if(c.medge1->next)
	    c.medge1->next->prev = c.medge1->prev;
	  
	  if (inOrder)
	  {
	    Edge* e1 = c.medge2->prev;
	    if(e1 && c.medge2)
	    {
	      Collision c2(e1,c.medge2);
	      pq.push(c2);  
	    }
	  }
	  else
	  {
	    Edge* e1 = c.medge2->next;
	    if (e1 && c.medge2)
	    {
	      Collision c2(e1,c.medge2);
	      pq.push(c2);
	    }	
	  }
	}
	else if (e1r2 < e2r2)
	{
	  bool inOrder = c.medge1->next == c.medge2;
	  c.medge2->visible = false;
	  if(c.medge2->prev)
	    c.medge2->prev->next = c.medge2->next;
	  if(c.medge2->next)
	    c.medge2->next->prev = c.medge2->prev;
	  if (inOrder)
	  {
	    Edge* e2 = c.medge1->next;
	    if (e2 && c.medge2)
	    {
	      Collision c2(e2,c.medge2);
	      pq.push(c2); 
	    }
	  }
	  else
	  {
	    Edge* e2 = c.medge2->prev;
	    if(e2 && c.medge2)
	    {
	      Collision c2(e2,c.medge2);
	      pq.push(c2);
	    }	
	  }
	}
	else
	{
	  // remove both.  This occurs when a number is a sum of two squares two different ways
	  c.medge1->visible = false;
	  if(c.medge1->prev)
	    c.medge1->prev->next = c.medge1->next;
	  if(c.medge1->next)
	    c.medge1->next->prev = c.medge1->prev;
	  c.medge2->visible = false;
	  if(c.medge2->prev)
	    c.medge2->prev->next = c.medge2->next;
	  if(c.medge2->next)
	    c.medge2->next->prev = c.medge2->prev;
	  // Don't create new collision
	}
      }
      else
      {
	cout << "only one is background!" << endl;
      }
    }
  }
}

