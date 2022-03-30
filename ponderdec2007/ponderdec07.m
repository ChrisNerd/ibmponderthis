u = 2^18;
p= zeros(1,u);
i = 1+floor((u-1)*rand())
p(i) = .5;
for k = 1:50
tt(k) = t(1,u,0,p);
tt4(k) = t4(1,u,0,p);
end
mean(tt4)
C = 5/(log(3)/log(2));
C * log(u)/log(2)