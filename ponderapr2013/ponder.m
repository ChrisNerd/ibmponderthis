upper=9
for d=0:upper
for o=1:upper
if (o==d)
continue
endif
for n=0:upper
if (n==o || n==d)
continue
endif
for t=1:upper
if(t==n || t==o || t==d)
continue
endif
for p=0:upper
if(p==t || p==n || p==o || p==d)
continue;
endif
for e=1:upper
if(e==p || e==t || e==n || e ==o || e==d)
continue
endif
for r=0:upper
if(r==e || r==p || r==t || r==n || r==o || r==d)
continue
endif
for i=1:upper
if(i==r || i==e || i==p || i==t || i==n || i==o || i==d)
continue
endif
for s=0:upper
if(s==i || s==r || s==e || s==p || s==t || s==n || s==o || s==d)
continue;
endif
for h=1:upper
if(h==s || h==i || h==r || h==e || h==p || h==t || h==n || h==o || h==d)
continue
endif
if (d/o+n/t == p/e + r/i +s/h)
d
o
n
t
p
e
r
i
s
h
endif
endfor
endfor
endfor
endfor
endfor
endfor
endfor
endfor
endfor
endfor

%first solution of 7561 lines of solution
%d = 0
%o =  2
%n =  5
%t =  1
%p =  6
%e =  9
%r =  7
%i =  3
%s =  8
%h =  4
%PONDER.THIS == 625097.1438


%400,000 lines of solution using 1:12
%last one is
%d =  12
%o =  10
%n =  11
%t =  6
%p =  4
%e =  8
%r =  1
%i =  5
%s =  7
%h =  3

