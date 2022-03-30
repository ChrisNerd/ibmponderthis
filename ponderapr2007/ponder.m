s = 100;
i = -s:2*s;
A = ones(1,length(i));
for b = 1:length(i)
for k=0:60
i(b);
q = mod(abs(2 - i(b)),3) + 1;
w = round(i(b)*q/3);
term = bincoeff(3*k + q, k + w) / 2^(3*k + q);
A(b) = A(b) * (1 - term);
end;
end;
plot(i, A);
