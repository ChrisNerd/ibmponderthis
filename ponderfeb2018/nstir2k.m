function x = nstir2k(a,b)
%NSTIR2K Stirling number(s) of the second kind.
%Stirling number of the second kind is the number of ways to partition a
%  set of n objects into k non-empty subsets and is denoted by S(k,i).
%  Stirling numbers of the second kind occur in the field of mathematics
%  called combinatorics and the study of partitions. In short words, it is
%  the number of ways to distribute k distinguishable elements into i
%  indistinguishable receptacles with no receptacle empty.
%
%  Stirling numbers of the second kind is one of two kinds of Stirling
%  numbers, the other kind being called Stirling numbers of the first 
%  kind, are the coefficients of powers of x in the polynomials [eg.
%  Q(x)=(x-1)*(x-2)*...*(x-n)].
%
%  It is called as it after the British mathematician James Stirling (1692-
%  1770).
%
%  In statistics, it is used in calculate (deriving) raw moments (non-
%  central) in some discrete distributions as Geometric, Binomial,
%  Negative Binomial, and Poisson, in a way that simplify the usual method
%  of deriving raw moments of higher order of an integer-valued random
%  variable by differentiate the generating function as many times as the
%  order of the moment requires.
%
%  They can be calculated using the following explicit formula:
%
%         S(k,i) = 1/i! * i=0_Sum_r * (-1)^(r-i) * rCi * i^k
%
%  where 0 =< r =< k and k are any non-negative integer. rCi, combination i
%  from r.
%   
%  Here we develp the m-file to generate the Stirling number of the second
%  kind. It is considered more informative than the previous m-files
%  generated before in this MCFE such as by Nikollaus Lorrell (13-11-2006)
%  and Luca (11-03-2011). This for it gives, not only a specific Stirling
%  number of second kind, but also (by default) the complete k partition. 
%
%  Syntax: function x = nstir2k(a,b) 
%      
%  Inputs:
%       a - set of n objects 
%       b - i non-empty subsets  
%  Outputs:
%       x  - Stirling number(s) of the second kind
%
%  Example 1. We need the S(4,3):
%
%  Calling on Matlab the function: 
%          x = nstir2k(4,3)
%
%  Answer is:
%
%  x = 6
%
%  Example 2. We need the S(4) [all the partition by default]:
%
%  Calling on Matlab the function: 
%          x = nstir2k(4)
%
%  Answer is:
%
%  x = 0     1     7     6     1
%
%  Created by A. Trujillo-Ortiz and R. Hernandez-Walls
%             Facultad de Ciencias Marinas
%             Universidad Autonoma de Baja California
%             Apdo. Postal 453
%             Ensenada, Baja California
%             Mexico.
%             atrujo@uabc.edu.mx
%
%  Copyright (C)  October 10, 2012.
%   
% To cite this file, this would be an appropriate format:
% Trujillo-Ortiz, A. and R. Hernandez-Walls. (2012). nstir2k:Stirling 
%    number(s) of the second kind. [WWW document].
%    URL http://www.mathworks.com/matlabcentral/fileexchange/
%    38597-nstir2k
%
%  References: 
%  Joarder, A. H. and Mahmood, M. (1997), An inductive derivation of the
%             Stirling numbers of the second kind and their application in
%             statistics. Journal of Applied Mathematics & Decision 
%             Sciences, 1(2):151-157. 
%

if  nargin < 2, %by default
    x = [];
    for b = 0:a,
        c = (-1)^b;
        t=[];
        for r = 0:b,
            d = (((-1)^r)*(r^a))/(factorial(b-r)*factorial(r));
            t = [t d];
        end
        x = [x c*sum(t)];
    end
else
    c = (-1)^b;
    s = [];
    for r = 0:b,
        d = (((-1)^r)*(r^a))/(factorial(b-r)*factorial(r));
        s = [s d];
    end
    x = c*sum(s);
end

return,