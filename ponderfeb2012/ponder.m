p=0.50009475707161049547037805477483714077890953654634897272221069808504359787953476;
p=1-p;
best=110;
bestp=1;
bestn=1;
for i=1:10^6
	n=round(p*i);
pstar=n/i;
if(abs(pstar-p)<best)
  bestp=i;
bestn=n;
best=abs(pstar-p);
end
end
bestn
bestp
best
