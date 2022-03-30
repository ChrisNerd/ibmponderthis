index = 1;
xplot= 0:.01:1;
for x=0:.01:1
p1wins(index) = 0;
	for i = 1:10000
		p1= rand();
if (p1 < x)
     p1 = rand();
end
p2 = rand();
if (p2 < .5)
p2 = rand();
end
if (p1 > p2)
p1wins(index) = p1wins(index) +1;
end
	endfor
index = index+1;
endfor


plot(xplot,p1wins/1000);
