// You can use a black box solver to answer the question: "is the correct answer at least X?"
// If the answer is positive - it costs you one cent, but if not - it costs you 10 cents. 
#include<vector>
#include<iostream>
#include<algorithm>
#include<limits>
using namespace std;

double calcE(vector<double>& E, int a, int b);


double getE(vector<double>& E, int i)
{
  double retval=0;
  if (E[i] != numeric_limits<double>::max())
    {
      retval = E[i];
    }
  else
    {
      retval = calcE(E,0,i);
    }
  // Memoize the result
  E[i] = retval;
  return retval;
}

double min(vector<double> c)
{
  double m = numeric_limits<double>::max();
  for (int i = 0; i < c.size(); i++)
    {
      if (c[i] < m)
	{
	  m=c[i];
	}
    }
  return m;
}


double minIndex(vector<double> c)
{
  double m = numeric_limits<double>::max();
  int mi = 0;
  for (int i = 0; i < c.size(); i++)
    {
      if (c[i] < m)
	{
	  m=c[i];
	  mi=i;
	}
    }
  return mi;
}

double calcE(vector<double>& E, int a, int b)
{
  if (a >= b)
    return 0;
  // interval a to b is the same as interval 0 to b-a.
  // let c = b-a
  // return minimum over i from i=1 to i=c of i/(c+1) * (10+E(i-1)) + (c+1-i)/(c+1)*(1+E(c-i))

  vector<double> candidates;
  int c = b-a;

  // There's no point testing if the answer is at least as high as "a" since we already know that's true
  // so X starts at 1.  It still is unknown if X is equal to b, so X can go all the way up to and equal c.
  for (int X =1 ; X <=c ; X++)
    {
      candidates.push_back(1.0*X/(c+1) * (10 +getE(E,X-1))+ 1.0*(c+1-X)/(c+1)*(1+getE(E,c-X)));
    }

  return min(candidates);
}


int main()
{
  int a = 51;
  int b = 20000; //150
  int n=b-a+1;
  vector<double> E(n,numeric_limits<double>::max());
  E[0]=0;
  cout <<  "Answer to Ponder this: " << calcE(E,51,150) << endl;
  calcE(E,a,b);
  cout << "Solution to subproblems:\n";

  for (int j =0; j < E.size() - 1; j++)
    {
      cout << E[j] << " " ;
    }
  cout << endl;

  cout << "Numerators (denominators are just the index starting at 1):\n";
  vector<double> Em1;
  for (int j =0; j < E.size() - 1; j++)
    {
      cout << E[j]*(j+1) << " " ;
      Em1.push_back(E[j]*(j+1));
    }
  cout << endl;

  cout << "Difference between successive numerators:\n";
  vector<double> diffs;
  for (int j =0; j < Em1.size() - 2; j++)
    {
      cout << Em1[j+1]-Em1[j] << " " ;
      diffs.push_back(Em1[j+1]-Em1[j]);
    }
  cout << endl;

  cout << "Group and count:\n";
  double currentdiff = diffs[0];
  int count = 0;
  for (int j =0; j < diffs.size(); j++)
    {
      if ((diffs[j] - currentdiff)*(diffs[j] - currentdiff) > .1)
	{
	  cout << currentdiff << " " << count << endl;
	  currentdiff = diffs[j];
	  count = 1;
	}
      else
	{
	  count++;
	}
    }
  vector<int> minIndices;
  for (int c=0; c < E.size(); c++)
    {
  vector<double> candidates;
  for (int X =1 ; X <=c ; X++)
    {
      candidates.push_back(1.0*X/(c+1) * (10 +getE(E,X-1))+ 1.0*(c+1-X)/(c+1)*(1+getE(E,c-X)));
    }

  minIndices.push_back(minIndex(candidates));
  cout << minIndices[c] << ", ";
    }


  return 0;
}

/*
Answer to Ponder this: 26.5
Solution to subproblems: 0 5.5 7.66667 9 10 10.8333 11.5714 12.25 12.8889 13.5 14.0909 14.6667 15.1538 15.6429 16.0667 16.4375 16.8235 17.1667 17.4737 17.75 18.0476 18.3182 18.5652 18.7917 19 19.2308 19.4444 19.6429 19.8276 20 20.1613 20.3437 20.5152 20.6765 20.8286 20.9722 21.1081 21.2368 21.3846 21.525 21.6585 21.7857 21.907 22.0227 22.1333 22.2391 22.3617 22.4792 22.5918 22.7 22.8039 22.9038 23 23.0926 23.1818 23.2857 23.386 23.4828 23.5763 23.6667 23.7541 23.8387 23.9206 24 24.0769 24.1667 24.2537 24.3382 24.4203 24.5 24.5775 24.6528 24.726 24.7973 24.8667 24.9342 25.013 25.0897 25.1646 25.2375 25.3086 25.378 25.4458 25.5119 25.5765 25.6395 25.7011 25.7614 25.8202 25.8889 25.956 26.0217 26.086 26.1489 26.2105 26.2708 26.3299 26.3878 26.4444 26.5 26.5545 26.6078 26.6602...

Numerators (denominators are just the index starting at 1): 0 11 23 36 50 65 81 98 116 135 155 176 197 219 241 263 286 309 332 355 379 403 427 451 475 500 525 550 575 600 625 651 677 703 729 755 781 807 834 861 888 915 942 969 996 1023 1051 1079 1107 1135 1163 1191 1219 1247 1275 1304 1333 1362 ...
Difference between successive numerators: 11 12 13 14 15 16 17 18 19 20 21 21 22 22 22 23 23 23 23 24 24 24 24 24 25 25 25 25 25 25 26 26 26 26 26 26 26 27 27 27 27 27 27 27 27 28 28 28 28 28 28 28 28 28 29 29 29 29 29 29 29 29 29 29 30 30 30 30 30 30 30 30 30 30 30 31 31 31 31 31 31 31 31 31 31 31 31 31 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 32 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33 33 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 34 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 35 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 36 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 37 38....
Group and count: 11 1
12 1
13 1
14 1
15 1
16 1
17 1
18 1
19 1
20 1
21 2
22 3
23 4
24 5
25 6
26 7
27 8
28 9
29 10
30 11
31 13
32 16
33 20
34 25
35 31
36 38
37 46
38 55
39 65
40 76
41 89
42 105
43 125
44 150
45 181
46 219
47 265
48 320
49 385
50 461
51 550
52 655
53 780
54 930
55 1111
56 1330
57 1595

0 11/2 23/3 36/4 50/5 65/6
closed form is 1/2*(i-1)*(i+20) / i for i <= 12
i=1 0
i=2 11/2
i=3 23/3
i=4 36/4 9
50/5 10
65/6 10.8333
81/7 11.5714
98/8 12.25
116/9 12.8889
135/10 13.5
155/11 14.0909
176/12 14.6667 check
198/13 nope != 15.1538.  should be 197/13
221/14 219/14
245/15 
270/16

11 1
12 1
13 1
14 1
15 1
16 1
17 1
18 1
19 1
20 1
21 2
22 3
23 4
24 5
25 6
26 7
27 8
28 9
29 10
30 11
31 13
32 16
33 20
34 25
35 31
36 38
37 46
38 55
39 65
40 76
41 89
42 105
43 125
44 150
45 181
46 219
47 265
48 320
49 385
50 461
51 550
52 655
53 780
54 930
55 1111
56 1330
57 1595
https://oeis.org/A017904
Expansion of 1/(1 - x^10 - x^11 - ...).
Generating function: (x-1)/(x-1+x^10)
For n>=1, a(n) = number of compositions of n in which each part is >=10.
*/
