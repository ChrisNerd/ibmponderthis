b = [1 -2 1]
a = [1 -2 4 -3]
[r, p, k, e] = residue(b,a)
     for n = 1:10
r(1)*p(1)^n + r(2)*p(2)^n
end
