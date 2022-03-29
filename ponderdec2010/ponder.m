b/(a+b+c)=.248
(c+2)/(a+b+c+2)=.251
0 = .248a + (.248-1)b + .248c
2 -2*.251 = .251a + .251b +(.251-1)c

r=[
0.24800  -0.75200   0.24800   0.00000
0.25100   0.25100   -0.74900   1.49800
]

rf=rref(r)
1.00000   0.00000  -1.99602   4.48803
0.00000   1.00000  -0.98805   1.480101

a>b
a>c

b=c= when b=c=123.83
so c<123.83
c=40	a=84	b=41


Short answer: Alice got 84 votes.

Long answer:
Because there were no ties, Charles came in last and moved to second position with 2 votes also with no ties, we can conclude that both those votes went to Charles and that there were no other candidates running.

The equation after the first counting is
b/(a+b+c)=.248
and after the second counting
(c+2)/(a+b+c+2)=.251

I put these equations in to an augmented matrix:
[ .248 -0.752 .248  |  0;
0.251 0.251 -0.749 | 1.498]

Then put the matrix in reduced row echelon form:
[1   0  -1.99602   4.48803
0   1  -0.98805   1.480101]
keeping many more significant digits.
This gives me equations for Alice and Bob based on the free variable Charles.
Namely
Alice = 4.48803 + 1.99602 Charles
Bob=1.480101 + 0.98805 Charles

Graphing the second equations lets me see that for Bob to beat Charles the first time occurs when b>c, so the intersection occurs when b=c=123.8, so c must be less than 123.8.

I used a spreadsheet to iterate through Alice's and Bob's counts based on Charles'.  Alice and Bob are then rounded and the percentages are calculated.  The percentages are rounded to the nearest promille and compared with 24.8 and 25.1.  The first occurance of both percentages matching is when a=84, b=41 and c=40.

Subsequent matches occur at or after a=202, b=99 and c=99, which violates the criteria that there were no ties.

Thanks for another fun puzzle.  I'm still working on the November challenge.

Chris Shannon
Calgary Canada
