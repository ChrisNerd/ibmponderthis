% solves part 4 of Dec 2007 ponder this
function retval = t4(a,b,n,p)
n
a
b
if (b-a)<=2
retval = n+3  % because it should take one more test even if we're testing a single part
else
b1 = floor(a + (b-a)/3);
b2 = floor(a + 2* (b-a)/3);
failed = 1;
while (failed == 1)
n = n+3;
  if (sum(p(a:b1)) > rand() )
    failed = 0;
    n = n - 2;
    retval = t4(a, b1,n,p);
  elseif (sum(p(b1:b2)) > rand() )
    n = n - 1;
failed = 0;
    retval = t4(b1, b2,n,p);
  elseif (sum(p(b2:b)) > rand() ) 
    failed = 0;
    retval = t4(b2, b,n,p);    
  endif
endwhile
endif
endfunction