function fout = f(S)
  S1 = S(1:6);
  S2 = S(7:12);
for i =1:6
for j= 1:6
products(1+(i-1)*6 + (j-1)) =  S1(i) * S2(j);
endfor
endfor


# For maximization
fout =  - max(products);

endfunction
