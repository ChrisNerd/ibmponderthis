//http://en.wikipedia.org/wiki/Multiset
/*
Short answer: 130.

Long answer -
I started by looking how you can count 120 different ways of choosing 3 from a set of 8.  Learnt about multisets, which let me see how quickly the problem grows from smaller dice sides.

I first noticed that all sides must have a unique number.

Also the smallest number must be 1.  Since all numbers must be positive integers, 0 is out.  If the smallest was greater than 1, we could shift all 8 numbers down by the same amount and the sum of 3 rolls would be simply be shifted 3 times that amount, and would still all be unique.

When thinking about rolling the dice twice, I saw that what we want is all the pairs to have a unique subtraction between them.  This was exactly like a Golomb ruler!  I thought of how the idea of a Golomb ruler could be extented to work for the case of 3 rolls but I couldn't see an easy way.

But searching for Golomb rulers is known to be a hard problem, so I didn't expect to find any mathematical shortcuts to this Ponder This.  But 8 is a realitively low number, and the hint of the highest being lower that 219 also limits the search space.  I went ahead and designed an algorithm based on dynamic programming.  I thought if I had the solution to a 7 sided dice, I could just plop on the the eighth side and quickly find allowed values for it.

I thought of using the Functional Programming paradym in Scala, since I had just taken a course in it, and since the problem seemed similar to examples from that course.  While using a nice recursive form would look so much nicer, I thought it'd was simple enough to just use familiar C++ and embed 8 nearly identical for loops.

My first attempt was overthinking the details and trying to minimize the work to check for each new dice side given a solution of smaller dice sides.  Checking 0, 1, 2 and 3 rolls of the new side seperately.  It didn't seem worth the added complexity than just checking all combinations from scratch each time.

Without losing generality, we can assume the sides 1 through 8 have increasing numbers.  The first side is has a 1.  The second side will be iterated through 2 to 219-7, (or 6 if we are searching for ties for best solution), and so on.
Before entering the next for loop, the current side is checked for feasibility of 3 rolls with unique sums.  This check is done using a simple hash, and checking for collisions upon insertions.  The hash is just an array of length 3 * current value initialized to zeros.  Non zeros are placed at all the sums of 3 distant elements.
When we reach the 8th side, and find a solution with 8 valid sides, we shrink the search space automatically by updating the maximum range of each of the other sides.  That is, the upper limit on the for loops is a variable based on the smallest 8th side found so far.
This pruning of the search space yields two solution quickly enough of 
1 3 6 35 75 108 121 130 
and its mirror (if it was a Golomb ruler, it would just be flipped)
1 10 23 56 96 125 128 130 

By putting a Dirac delta at each mark and zeroes elsewhere, I examined the sequence by plotting them in Octave.  The autocorrelation of the sequence resulted in the sums of 1 roll, and another convolution resulted in the sums of 3 rolls.  Since there are 6 different ways of rolling the same 3 numbers, the maximum value of 6 indicates that there is only 1 unique set of 3 numbers that sum to each value for all values.

Unfortunately, I can't see any inherent mathematical pattern in these numbers.  They don't seem to be based on primes or anything.  Maybe the Golomb ruler people are familiar with them.

Thanks for another fun Ponder This,

Chris Shannon
Calgary Canada

new n8max 218 from 1 2 5 14 33 72 137 218 
new n8max 216 from 1 2 5 14 33 72 163 216 
new n8max 196 from 1 2 5 14 33 75 128 196 
new n8max 185 from 1 2 5 14 33 85 131 185 
new n8max 152 from 1 2 5 14 34 82 125 152 
new n8max 151 from 1 2 5 15 44 92 114 151 
new n8max 145 from 1 2 5 21 76 102 136 145 
new n8max 142 from 1 2 5 40 84 112 130 142 
new n8max 141 from 1 2 10 38 88 109 135 141 
new n8max 140 from 1 2 14 87 102 123 133 140 
new n8max 137 from 1 2 15 21 85 96 133 137 
new n8max 134 from 1 2 19 34 78 124 130 134 
n[1] = 3
new n8max 130 from 1 3 6 35 75 108 121 130 
new n8max 130 from 1 10 23 56 96 125 128 130 


a=[1 3 6 35 75 108 121 130];
x=zeros(1,130);
x(a)=1;
y=conv(x,x);
z=conv(y,x);
figure
subplot(311)
plot(x)
subplot(312)
plot(y)
subplot(313)
plot(z)




*/
#include <iostream>
#include <vector>
#include <math.h>
using namespace std;

bool check(int p, vector<int>& n)
{
  vector<int> a(n[p]*3+1,0);
  for (int a1 = 0; a1 <=p; a1++)
    {
      for (int a2 = a1; a2 <=p; a2++)
	{
	  for (int a3 = a2; a3 <=p; a3++)
	    {
	      int k = n[a1]+n[a2]+n[a3];
	      if (a[k] != 0)
		return false;
	      a[k]=1;
	    }
	}
    }
  return true;

}

bool allowed1roll(int p, vector<int>& n, vector<int>& vc)
{
  for (int i =0; i <= p; i++)
    {
      for (int j=0; j < vc.size(); j++)
	{
	  if (n[p] + n[i] == vc[j])
	    {
	      cout << p <<" " << i << " " <<n[p] << " "<< n[i] << " " << j << endl;
	    return false;
	    }
	}
      vc.push_back(n[p] + n[i]);
    }
  return true;
}

// returns if you can insert 1 roll of n given that the
// you have two rolls that add to the entries in v2
// and three rolls that add to the entries in v3
bool allowed(int n, vector<int>& v2, vector<int>& vc)
{
  for (int i = 0; i < v2.size(); i++)
    {
      for (int j =0; j < vc.size(); j++)
	{
	  if (n + v2[i] == vc[j])
	    {
	      cout << "Allowed " << n << " " << v2[i] << " " << i << endl;
	      return false;
	    }
	}
      vc.push_back(n+v2[i]);
    }
  return true;
}

bool allowed2rolls(int p, vector<int>& n, vector<int>& vc)
{
  for (int i =0; i < p; i++)
    {
      for (int j=0; j < vc.size(); j++)
	{
	  if (2*n[p] + n[i] == vc[j])
	    {
	      cout << "allowed2rolls " << p <<" " << i << " " <<n[p] << " "<< n[i] << " " << j << endl;    
	      return false;
	    }
	}
      vc.push_back(2*n[p] + n[i]);
    }
  return true;
}


int main()
{
  vector<int> a(16384*3+1,0);

  for (int a1 = 0; a1 <=7; a1++)
    {
      for (int a2 = 0; a2 <=7; a2++)
	{
	  for (int a3 = 0; a3 <=7; a3++)
	    {
	      a[ pow(4,a1) + pow(4,a2) + pow(4,a3)] ++;
	    }
	}
    }

  int count = 0;
  for (int c=0; c < a.size(); c++)
    {
      if (a[c] != 0)
	{
	  cout << c << " " << a[c] << endl;
	  count++;
	}

    }
  cout << count << endl;
  
  //  int n8max = 219;
  int n8max = 16384;
  vector<int> n(8);
  for (n[0]=1; n[0] <= 1; n[0]++)
    {      
      cout << "n[0] = " << n[0]<< endl;

      vector<int> n02;
      vector<int> n03;
      // after 2 rolls it will certainly have only 1 entry, 2.
      n02.push_back(2);
      // after 3 rolls it will certainly have only 1 entry, 3.
      n03.push_back(3);
      for (n[1]=n[0]+1; n[1] <=n8max-6; n[1]++)
	{      
	  cout << "n[1] = " << n[1]<< endl;
	  // what are the possible values of n[1] given we have
	  // all the combos of 2 rolls of the previous number of sides (n02)
	  // all three rolls of the previous number of sides (n03)
	  /*	  vector<int> n12 = n02;
	  vector<int> n13 = n03;
	  if (!allowed1roll(1, n, n12))
	    continue;
	  if (!allowed(n[1], n12, n13))
	    continue;
	  if (!allowed2rolls(1, n, n13))
	    continue;
	  // This one will always be fine because n[1] is the highest number so far
	  n13.push_back(3*n[1]);
	  */
	  if(!check(1,n))
	    continue;
      for (n[2]=n[1]+1; n[2] <=n8max-5; n[2]++)
	{      
	  /*	  cout << "n[2] = " << n[2]<< endl;
	  vector<int> n22 = n12;
	  vector<int> n23 = n13;
	  if (!allowed1roll(2, n, n22))
	    continue;
	  if (!allowed(n[2], n22, n23))
	    continue;
	  if (!allowed2rolls(2,n, n23))
	    continue;
	  n23.push_back(3*n[2]);
	  */
	  if(!check(2,n))
	    continue;
      for (n[3]=n[2]+1; n[3] <=n8max-4; n[3]++)
	{      
	  /*
	  vector<int> n32 = n22;
	  vector<int> n33 = n23;
	  if (!allowed1roll(3, n, n32))
	    continue;
	  if (!allowed(n[3], n32, n33))
	    continue;
	  if (!allowed2rolls(3,n,n33))
	    continue;
	  n33.push_back(3*n[3]);
	  */
	  if(!check(3,n))
	    continue;
      for (n[4]=n[3]+1; n[4] <=n8max-3; n[4]++)
	{      
	  /*
	  vector<int> n42 = n32;
	  vector<int> n43 = n33;
	  if (!allowed1roll(4, n, n42))
	    continue;
	  if (!allowed(n[4], n42, n43))
	    continue;
	  if (!allowed2rolls(4,n,n43))
	    continue;
	  n43.push_back(3*n[4]);
	  */
	  if(!check(4,n))
	    continue;
      for (n[5]=n[4]+1; n[5] <=n8max-2; n[5]++)
	{      
	  /*
	  vector<int> n52 = n42;
	  vector<int> n53 = n43;
	  if (!allowed1roll(5, n, n52))
	    continue;
	  if (!allowed(n[5], n52, n53))
	    continue;
	  if (!allowed2rolls(5,n,n53))
	    continue;
	  n53.push_back(3*n[5]);
	  */
	  if(!check(5,n))
	    continue;
      for (n[6]=n[5]+1; n[6] <=n8max-1; n[6]++)
	{      
	  /*
	  vector<int> n62 = n52;
	  vector<int> n63 = n53;
	  if (!allowed1roll(6, n, n62))
	    continue;
	  if (!allowed(n[6], n62, n63))
	    continue;
	  if (!allowed2rolls(6,n,n63))
	    continue;
	  n63.push_back(3*n[6]);
	  */
	  if(!check(6,n))
	    continue;
	  for (n[7]=n[6]+1; n[7] <=n8max/*-1*/; n[7]++)
	{      
	  /*
	  vector<int> n72 = n62;
	  vector<int> n73 = n63;
	  if (!allowed1roll(7, n, n72))
	    continue;
	  if (!allowed(n[7], n72, n73))
	    continue;
	  if (!allowed2rolls(7,n,n73))
	    continue;
	  n73.push_back(3*n[7]);
	  */
	  if(!check(7,n))
	    continue;
	  n8max=n[7];
	  cout << "new n8max " << n8max << " from " ;
	  for (int k=0; k < 8; k++)
	    {
	      cout << n[k] << " " ;
	    }
	  cout << endl;
	}}}}}}}      

    }
}
