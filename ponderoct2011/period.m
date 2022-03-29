function retval = period(n, amax)
  if( mod(n,2)== 0)
    retval = period(n/2, amax);
return
endif
if( mod(n,5)== 0)
retval =period(n/5, amax);
return;
endif
 for m=1:amax
 if( mod(10^m-1, n) == 0)
   retval = m;
 return ; 
endif
endfor
retval = -1;
return ; 
end


