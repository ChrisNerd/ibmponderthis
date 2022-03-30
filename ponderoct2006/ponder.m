totalarea=0;
numiter=10000;
for iter=1:numiter;
x=rand;
y=rand;
if y>x
temp=y;
y=x;
x=temp;
end
x1=x;
y1=y;


x=rand;
y=rand;
if y>x
temp=y;
y=x;
x=temp;
end
x2=x;
y2=y;


x=rand;
y=rand;
if y>x
temp=y;
y=x;
x=temp;
end
x3=x;
y3=y;
totalarea= totalarea + 1/2 * norm(cross([x3-x2,y3-y2,0],[x3-x1,y3-y1,0]));

end
averagearea = totalarea/numiter / .5
