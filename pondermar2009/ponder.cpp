// Chris Shannon - Calgary Canada
// March 2009 IBM Ponder This

#include <iostream>

void pc(int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
void pchar(int);
bool check(int,int,int,int,int,int,int,int,int,int,int,int,int,int,int);
using namespace std;
int main()
{
  for (int a = 1; a <= 15; a++)
    {
      cout << "a = " << a << endl;
      for (int b = 1; b <= 15; b++)
	{
	  if (b == a)
	    continue;
	  for (int c = 1; c <= 15; c++)
	    {
	      if (c == b || c == a)
		continue;
	      for (int d = 1; d <= 15; d++)
		{
		  if (d == c || d == b || d == a)
		    continue;
		  int e = (a + b + c + d) % 16;
		  if (e == 0 || e == d || e== c || e == b || e == a)
		    continue;
		  for (int f = 1; f <= 15; f++)
		    {
		      if (f == e|| f == d || f == c || f == b || f == a)
			continue;
		      for (int g = 1; g <= 15; g++)
			{
			  if ( g == f || g == e|| g == d || g == c || g == b || g == a)
			    continue;
			  for (int h = 1; h <= 15; h++)
			    {
			      if ( h == g || h == f || h == e|| h == d || h == c || h == b || h == a)
				continue;
			      int t1 = (a + b + c + d) / 16;
			      int i = 16 - (t1 + f + g + h) % 16;
			      if ( i == 16 )
				continue;
			      if ( i == h || i == g || i == f || i == e|| i == d || i == c || i == b || i == a)
				continue;
			      for (int j = 1; j <= 15; j++)
				{
				  if ( j == i || j == h || j == g || j == f || j == e|| j == d || j == c || j == b || j == a)
				    continue;
				  for (int k = 1; k <= 15; k++)
				    {
				      if ( k == j || k == i || k == h || k == g || k == f || k == e|| k == d || k == c || k == b || k == a)
					continue;
				      int l = ((t1 + f + g + h + i)/16 + j + k)%16;
				      if ( l == k || l == j || l == i || l == h || l == g || l == f || l == e|| l == d || l == c || l == b || l == a)
					continue;
				      int m = ((((e+l)/16 + d + i)/16 + c + h)/16 + b + g + k)/16 + a + f + j;
				      if ( m >= 16 || m == l || m == k || m == j || m == i || m == h || m == g || m == f || m == e|| m == d || m == c || m == b || m == a)
					continue;
				      int n = ((((e+l)/16 + d + i)/16 + c + h)/16 + b + g + k) % 16;
				      if ( n == m || n == l || n == k || n == j || n == i || n == h || n == g || n == f || n == e|| n == d || n == c || n == b || n == a)
					continue;
				      int o = (e + l)%16;
				      if ( o == m || o == n || o == l || o == k || o == j || o == i || o == h || o == g || o == f || o == e|| o == d || o == c || o == b || o == a)
					continue;
				      
				      if (check(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o))
					pc(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o);
				    }
				}
			    }
			}
		    }
		}
	    }
	}
    }
}

void pc(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o)
{
  pchar(a);
  pchar(b);
  pchar(c);
  pchar(d);
  pchar(e);
  cout << endl;
  pchar(f);
  pchar(g);
  pchar(h);
  pchar(i);
  cout << "0" << endl;
  pchar(j);
  pchar(k);
  cout << "00";
  pchar(l);
  cout << endl;
  pchar(m);
  pchar(n);
  cout << "00";
  pchar(o);
  cout << endl << endl;
}

void pchar(int a)
{
  if (a < 10)
    cout << a;
  else if (a == 10)
    cout << "A";
  else if (a == 11)
    cout << "B";
  else if (a == 12)
    cout << "C";
  else if (a == 13)
    cout << "D";
  else if (a == 14)
    cout << "E";
  else if (a == 15)
    cout << "F";
  else
    cout << "error";
}

bool check(int a, int b, int c, int d, int e, int f, int g, int h, int i, int j, int k, int l, int m, int n, int o)
{
  int fc = 0; // false count
  int t1 = e + l;
  if ( t1 % 16 != o)
    fc++;
  
  int t2 = t1/16 + d + i;
  if ( t2 % 16 != 0)
    fc++;
  
  int t3 = t2/16 + c + h;
  if (t3 % 16 != 0)
    fc++;
  
  int t4 = t3/16 + b + g + k;
  if (t4 % 16 != n)
    fc++;
  
  int t5 = t4/16 + a + f + j;
  if (t5 != m)
    fc++;
  
  
  int t6 = a + b + c + d;
  if ( t6 % 16 != e)
    fc++;
  
  int t7 = t6/16 + f + g + h + i;
  if (t7 % 16 != 0)
    fc++;
  
  int t8 = t7/16 + j + k;
  if (t8 % 16 != l)
    fc++;
  
  int t9 = t8/16 + m + n;
  if (t9 != o)
    fc++;
  
  if (fc == 0)
    return true;
  return false;
}
