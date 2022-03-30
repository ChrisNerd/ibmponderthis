%subset(X, [1,3,4]).        % backtracks over all subsets
between(1, b, a).
between(a, c, b).
between(b, d, c).
between(c, 12000, d).
%sq(X) :- nat(Y), X is Y*Y
%squareNumber(X) :- sqrt(X) mod 1 == 0
%or do
%squareNumber(X) :- subset(X, (1 to 220).map(x=>x*x))
%squareNumber(a+b).
%squareNumber(a+c).
%squareNumber(a+d).
%squareNumber(b+c).
%squareNumber(b+d).
%squareNumber(c+d).
%squareNumber(a+b+c+d).
