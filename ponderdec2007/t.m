% solves part 3 of Dec 2007 ponder this
function retval = t(a,b,n,p)
n
a
b
if (b-a)==0
retval = n+1  % because it should take one more test even if we're testing a single part
else
failed = 1;
while (failed == 1)
n = n+2;
  if (sum(p(a:(a+b-1)/2) > rand() ) )
    failed = 0;
    n = n - 1;
    retval = t(a, (a+b-1)/2,n,p);
  elseif (sum(p((a+b+1)/2:b) > rand() ) )
    failed = 0;
    retval = t((a+b+1)/2, b,n,p);
  endif
endwhile
endif
endfunction