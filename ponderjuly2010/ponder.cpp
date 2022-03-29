#include <iostream>
#include <math.h>
int main()
{
   // The ring buffer storing the sequence
   long long int x[] = {3,9,24,-1};

   // The index counter, starting at element x[3] = -1 (invalid)
   // Since C++ arrays are 0 indexed, n == i + 1
   long long unsigned int i = 3;

   // The working in mod 1,000,000,000
   const unsigned int d = static_cast<unsigned int>(pow(10,9));

   const unsigned int progressIncrements = static_cast<unsigned int>(pow(10,9));
  
   // Modulus of negative numbers is implementation dependent, the sequence
   // may have negative numbers but it should not effect the outcome.
   int a = (-1)% 5;
   fprintf(stdout, "a=%i\n",a);
   fprintf(stdout, "sizeof(int) = %i\n", sizeof(int));
   fprintf(stdout, "sizeof(long unsigned int) = %i\n", sizeof(long unsigned int));
   fprintf(stdout, "sizeof(long long int) = %i\n", sizeof(long long int));
   fprintf(stdout, "sizeof(long long unsigned int) = %i\n", sizeof(long long unsigned int));
   fprintf(stdout, "sizeof(x[0]) = %i\n", sizeof(x[0]));

   while(true)
   {
      // The recurrence relation (mod d)
      // ---- Adding d does not change its value (mod d) but does keep it from
      // being negative and the modulus operator giving incorrect results.
      long long int rawint = 3*x[(i-1)%4] - x[(i-3)%4] + d;
      x[i%4] = (rawint)%d;
      if (i%progressIncrements == 0 || i <= 40)
      {
         fprintf(stdout, "Progress... x[%lli] = %lli\n",
            i,x[(i-1)%4]);
      }

      // This test fails if rawint is 4 bytes or if we didn't add d to rawint;
      if (rawint < 0)
      {
         fprintf(stdout, "Negative: %lli\nrawint = %lli\n x[i] = %lli\n", i+1, rawint, x[i%4]);
      }

      if (x[i%4] == 0)
      {
         fprintf(stdout, "\n\nSOLUTION FOUND!!!\ni = %lli\n\n", i+1);
        // break;
      }
      i++;
   }
}
