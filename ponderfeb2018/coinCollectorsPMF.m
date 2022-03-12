function pNequalsn = coinCollectorsPMF(n)

pNequalsn = [1];
for i=1:n
%pNequalsn=fftconv(pNequalsn, geopdf(0:n*10, i/n));
pNequalsn=conv(pNequalsn, geopdf(0:n*10, i/n));
end

pNequalsn = [zeros(1,n-1) pNequalsn];
expectn = sum((1:length(pNequalsn)).*pNequalsn);

pNGreaterOrEqualsn = cumsum(pNequalsn);
