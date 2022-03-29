for n=1:50000
	periods(n) = period(n, 500);
end

p8 = find(8==periods);

for i=1:length(p8)
	maxfactor(i)=max(factor(p8(i)));
end
maxfactor
