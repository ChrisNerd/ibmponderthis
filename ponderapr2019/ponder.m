%m =[[13 19 37]
%[67 7 109]
%[43 103 31]]

m= [[11 17 23]
[ 47 5  59]
[ 29 71 41]]


sum(m)/3
sum(m')/3
m(1,1)
(m(1,1)+m(2,2)+m(3,3))/3
(m(3,1)+m(2,2)+m(1,3))/3

isprime(sum(m)/3)
isprime(sum(m')/3)
isprime((m(1,1)+m(2,2)+m(3,3))/3)
isprime((m(3,1)+m(2,2)+m(1,3))/3)

% All 8 sums, for both cases are verified to be prime