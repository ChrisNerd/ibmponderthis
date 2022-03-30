m = 10;
for n = 1:m
x(n) = bincoeff(m,n) * (3*n - m); % / 2^m;
end