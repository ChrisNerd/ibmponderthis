a=zeros(1,1000000); % boolean values, no integers have been visited
b = 10;  % frog's current position
a(b)=1;
for c=1:1500000  % dummy counter
if (rand < .5)
b=b+2;  % frog jumps forward 2
else
b = b -1; % frog falls back 1
end
a(b)=1;  % either case interger 'b' is marked as visited
end
%plot(a)
b
1-sum(a)/b  % sum(a) is the total number of visited integers, b is roughly the highest value.  We'll ignore the start value