i=[2:166];
for j=1:165
	 k=factor(i(j));
	 l(j)=k(1);
end
unique (l)
