function retval = con8prod(n)
retval=1;
for i=1:8
a0 = floor(10*n);
n = 10*n - a0;
retval=retval*a0;
endfor
retval1 = retval;
retval2 = 1;
for i=1:8
a0 = floor(10*n);
n = 10*n - a0;
retval2=retval2*a0;
endfor

if (retval1 == retval2)
  retval=retval1;
 else
   retval = -1;
endif
