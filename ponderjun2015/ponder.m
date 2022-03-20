%
format long
clear all;
a(1)=100i;
a(2)=-100i;

for k=3:9
 s=std(a(1:k-1),1);
 m=mean(a(1:k-1));
 r(k)=2*pi*rand;
 a(k)=fix(m+fix(3*s*exp(i*r(k))));
endfor

calcarea(a);

% now it's initialized
% find out which point to move that will have the greatest impact on the area
% (or maybe the greatest impact on the standard deviation)
z=1;
for w=1:30
 amax=0;
 bestk=0;
 bestr=-1;

 for k=3:9
  for r1=linspace(0,2*pi,100)
  temp=a;
  a(k)=fix(mean(a(1:k-1))+fix(3*std(a(1:k-1),1)*exp(i*r1))); 

  %if a(k) is changed, then all the following As need to be changed too.
  for k1=k+1:9
   a(k1)=fix(mean(a(1:k1-1))+fix(3*std(a(1:k1-1),1)*exp(i*r(k1))));
  endfor

  if(calcarea(a) > amax)
   bestk=k;
   bestr=r1;
   amax=calcarea(a);
  endif
  % need to undo all the damage to the rest of the As
  a=temp;
  endfor
 endfor

 a(bestk)=fix(mean(a(1:bestk-1))+fix(3*std(a(1:bestk-1),1)*exp(i*bestr)));
 r(bestk)=bestr;
 for k1=bestk+1:9
  a(k1)=fix(mean(a(1:k1-1))+fix(3*std(a(1:k1-1),1)*exp(i*r(k1))));
 endfor

 bests(z)=calcarea(a);
 z++

 if (calcarea(a) > 7700000)
  a
 endif

endfor

a
calcarea(a);

for k=3:9
 s=std(a(1:k-1),1)
 m=mean(a(1:k-1))
 abs(a(k)-m)/s
endfor

%one result
%a =

% Columns 1 through 7:

 %     0 +  100i     -0 -  100i    300 +    0i    384 +  399i    934 +   51i    377 + 1233i   1953 -   31i

 %Columns 8 and 9:

 % -1745 +  163i     66 - 2916i

%at =

% Columns 1 through 7:

%   -100 +    0i    100 -    0i      0 +  300i   -399 +  384i    -51 +  934i  -1233 +  377i     31 + 1953i

% Columns 8 and 9:

%   -163 - 1745i   2916 +   66i

%(-100,0),(100,0),(0,300),(-399,384),(-51,934),(-1233,377),(31,1953),(-163,-1745),(2916,66)
%area  7701668
