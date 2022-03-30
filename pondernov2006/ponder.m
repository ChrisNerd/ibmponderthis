clear all;
greatestk = 0;
inlistall=nchoosek(1:16,4);
for m=1:length(inlistall)
inlist=inlistall(m,:);
inlistperms=perms(inlist);
possible=zeros(1,max(inlist)^4);
for k=1:24
a=inlistperms(k,1);
b=inlistperms(k,2);
c=inlistperms(k,3);
d=inlistperms(k,4);

r=a-b-c-d; #3 subtracted
validdater;
r=a+b-c-d; #2 subtracted
validdater;
r=a+b+c-d; #1 subtracted
validdater;
r=a+b+c+d; #0 subtracted
validdater;

r=a+b+c*d;
validdater;
r=a+b-c*d;
validdater;
r=a-b+c*d;
validdater;
r=a-b-c*d;
validdater;

r=a*b+c*d;
validdater;
r=a*b-c*d;
validdater;
r=a+b*c*d;
validdater;
r=a-b*c*d;
validdater;

r=a+b*(c+d);
validdater;
r=a-b*(c+d);
validdater;
r=a+b*(c-d);
validdater;
r=a-b*(c-d);
validdater;

r=a*b*(c+d);
validdater;
r=a*b*(c-d);
validdater;

r=a*(b+c+d);
validdater;
r=a*(b+c-d);
validdater;
r=a*(b-c-d);
validdater;

r=a*(b*c+d);
validdater;
r=a*(b*c-d);
validdater;

r=(a+b)*(c+d);
validdater;
r=(a-b)*(c+d);
validdater;
r=(a+b)*(c-d);
validdater;
r=(a-b)*(c-d);
validdater;

r=a*b*c*d;
validdater;
end


k=1;
while(possible(k))
k=k+1;
end
if (k > greatestk)
inlist
k
greatestk = k;
end
end
