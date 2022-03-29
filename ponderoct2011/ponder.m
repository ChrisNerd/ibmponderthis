factor (99999999)
#3^2×11×73×101×137  (6 prime factors, 5 distinct)
a=1:8;
p=perms(a);
v = [10^0, 10^1, 10^2, 10^3, 10^4, 10^5, 10^6, 10^7]';

for i=1:length(p)
n(i) = p(i,:)*v;
end

for i=1:length(n)
smooths(i)=max(factor(n(i)));
end
[m1 m2] = min(smooths)
#m1 =  11
#m2 =  7226
cands = n(11==smooths)
68457312   46835712   12835746   16238475   23417856   17465382   35126784   21346578

for i=1:length(cands)
factor(cands(i))
end

for i=1:length(n)
n2(i) = n(i)/gcd(n(i),99999999);
n7(i) = 99999999/gcd(n(i),99999999);
end

n(21==n2)
ans =  15328467

21/137 = 3 7
1/(1/7+1/3+1+1+1+1+1+1) = 21/136
39/137 = 3 13
45 = 3 3 5
64 = 2 2 2 2 2 2
73 = 1 73
92 = 2 2 23
98 = 2 7 7
116 = 2 2 29
all the ones over 137 (above) after that they are all over 10001 or higher)


n3=[n2; n]

[s1 s2] = sort (n3')

n4=n2(s2(:,1))

for i=1:length(n4)
factor(n4(i))
end

for i=1:length(n4)
smoothn4(i)=max(factor(n4(i)));
end
[s5 s6] = sort (smoothn4)

n4(s6)

The fifth harmonic number is \frac{137}{60}
