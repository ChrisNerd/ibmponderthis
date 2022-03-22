#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <vector>
#include <algorithm>
#include <math.h>

using namespace std;
bool s(int d, vector<int> n, vector<int> e);
bool isSolvingPossible(int d, vector<int> n, vector<int> e);
int* t;
int q;
int fileNumber;
void writeSVG();
int main(int argc, char* argv[])
{
  fileNumber = 0;
  q=5;
  if (argc == 2)
    q = atoi(argv[1]);

  vector<int> n(pow(2,q)-1);
  vector<int> e(pow(2,q)-2);
  t= new int[(int) pow(2,q)-1];
  for (int i =0; i < pow(2,q)-2;i++)
    {
      n[i]=i+1;
      e[i]=i+1;
    }
  n.back() = pow(2,q)-1;
  t[0]=3;
  t[1]=30;
  t[2]=31;
  t[3]=28;
  t[4]=29;
  t[5]=1;
  t[6]=2;
  n.clear();
  for (int i=0; i<28;i++)
    {
      n.push_back(i);
    }
  e.clear();
  for (int i=3; i<27;i++)
    {
      e.push_back(i);
    }

  s(7,n,e);
}

bool s(int d, vector<int> n, vector<int> e)
{
  /*  if (d==0)
    {
      t[0]=1;
      vector<int> nc(pow(2,q)-2);
      for (int i =0; i<nc.size();i++)
	nc[i]=n[i+1];
      return s(1,nc,e);
    }
    else*/ if (d==pow(2,q)-1)
    {
      for (int i = 0; i < pow(2,q)-1;i++)
	{
	  cout << t[i] << " " ;
	}
      cout << endl;
      //      writeSVG();
      return true;
    }
  /*
  else if (d==2)
    {
      t[d] = pow(2,q)-1;
      n.erase(remove(n.begin(), n.end(), t[d]), n.end());
      e.erase(remove(e.begin(), e.end(), pow(2,q)-2), e.end());
      return s(3,n,e);
    }
  */

    if (d>2 && !isSolvingPossible(d,n,e))
    {
        return false;
    }
  for(int i=0; i<n.size();i++)
    {
      if (d != 0 && d%2==0 && t[d-1] > n[i])
	{
	  continue;
	}
      /*   if (d==1 && n[i]==2 && q!=2)
      	continue;
      if (d==1 && q!=2 && n[i]==pow(2,q)-1)
	continue;
      */
      vector<int> nc=n;
      vector<int> ec=e;
      t[d]=n[i];
      nc.erase(remove(nc.begin(), nc.end(), t[d]), nc.end());
      int parent=(d-1)/2;
      int ev = abs(t[d]-t[parent]);
      if (d!=0 && !binary_search(e.begin(), e.end(), ev))
	{
	  continue;
	}
      else
	{
	  ec.erase(remove(ec.begin(), ec.end(), ev), ec.end());
	  s(d+1,nc,ec);
	}
    }
}


bool isSolvingPossible(int d, vector<int> n, vector<int> e)
{
  // 2 cases, d < 31 pow(2,q-1)-1 or else
  // first case you get at least one opportunity to use any combination of ni-nj
  // second case you can only match ni with np
  if (d < pow(2,q-1)-1)
    {
      // if any ei can not be made by any combination of ni-np return false
      vector<int> ec = e;
      for (int i =0; i < n.size() && !ec.empty();i++)
	{
	  // set of parents is from (d-1)/2 to d-1
	  for (int j=(d-1)/2; j <=d-1; j++)
	    {
	      ec.erase(remove(ec.begin(),ec.end(),abs(n[i]-t[j])), ec.end());
	      if (ec.empty())
		{ 
		  break;
		}
	    }
	  // set of other remaining nodes
	  for (int j=i+1; j < n.size() && !ec.empty(); j++)
	    ec.erase(remove(ec.begin(),ec.end(),abs(n[i]-n[j])), ec.end());
	  if (ec.empty())
	    {
	      break;
	    }
	    
	}
      if (!ec.empty())
	{
	  return false;
	}

      // if any ni has no parent such that abs(ni-np) is not a member of e
      for (int i =0; i < n.size();i++)
	{    
	  bool foundSuitableParent=false;
	  // set of parents is from (d-1)/2 to d-1
	  for (int j=(d-1)/2; j <=d-1; j++)
	    {
	      if (binary_search(e.begin(), e.end(),abs(n[i] - t[j])))
		{
		  foundSuitableParent = true;
		  break;
		}
	    }
	  // set of other remaining nodes
	  for (int j=i+1; j < n.size() && !foundSuitableParent; j++)
	    {
	      if (binary_search(e.begin(), e.end(),abs(n[i] - n[j])))
		{
		  foundSuitableParent = true;
		  break;
		}
	    }
		
	  if (!foundSuitableParent)
	    {
	      return false;
	    }	    
	}
      return true;
    }
  // d >= pow(2,q-1)-1) // on the last row
  else
    {
      // if any ei can not be made by any combination of ni-np return false
      vector<int> ec = e;
      for (int i =0; i < n.size() && !ec.empty();i++)
	{
	  // set of parents is from (d-1)/2 to end of previous row
	  for (int j=(d-1)/2; j <=pow(2,q-1)-2; j++)
	    {
	      ec.erase(remove(ec.begin(),ec.end(),abs(n[i]-t[j])), ec.end());
	      if (ec.empty())
		{ 
		  break;
		}
	    }	    
	}
      if (!ec.empty())
	{
	  return false;
	}

      // if any ni has no parent such that abs(ni-np) is not a member of e
      for (int i =0; i < n.size();i++)
	{    
	  bool foundSuitableParent=false;
	  // set of parents is from (d-1)/2 to d-1
	  for (int j=(d-1)/2; j <=pow(2,q-1)-2; j++)
	    {
	      if (binary_search(e.begin(), e.end(),abs(n[i] - t[j])))
		{
		  foundSuitableParent = true;
		  break;
		}
	    }
		
	  if (!foundSuitableParent)
	    {
	      return false;
	    }	    
	}
      return true;
    }
}

  void writeSVG()
  {
    ofstream myfile;
    string f = "svg";
    ostringstream oss;
    oss << setw(4) << setfill('0') << fileNumber;
    f.append(oss.str());
    f.append(".svg");
      
    myfile.open(&f[0]);
    myfile << "<svg width=\"200px\" height=\"200px\" viewBox=\"0 0 17 17\">\n";

    for(int i =1; i < pow(2,q)-1;i++)
    {
      int p = (i-1)/2;
      int ex=t[i];
      int ey=t[p];
      if (ex > ey)
      {
	swap<int>(ex,ey);
      }
      
      myfile << "<rect x=\"" << ex << "\" y=\""<< ey << "\" width=\"1\" height=\"1\" fill=\"none\" stroke=\"blue\" stroke-width=\".2\"/>\n";      
    }
//    <rect x="1" y="1" width="1198" height="398"
//        fill="none" stroke="blue" stroke-width="2"/>  
        myfile << "\n </svg>";
    
    myfile.close();
      fileNumber++;
      };
