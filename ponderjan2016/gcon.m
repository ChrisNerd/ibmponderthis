function gout = gcon(S)
  S1 = S(1:6);
  S2 = S(7:12);
for i =1:6
for j= 1:6
products(1+(i-1)*6 + (j-1)) =  S1(i) * S2(j);
endfor
endfor
products = sort(products);

penalty1=0;
for i=1:35

######################################
# CHANGE the constant on the next line!!!
######################################
if (products(i+1) <= 65 && (products(i+1) - products(i) > 3))
penalty1+=(products(i+1) - products(i) -3);
endif
endfor
    penalty2=0;
    k=min(products);
if (k > 2)
    penalty2 =  k - 2;
    endif
gout = penalty1 + penalty2;
endfunction
