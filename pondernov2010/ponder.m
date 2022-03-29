function [out1] = f(i,j,k,l,m)
  if ((i-m)*(j-m) == 0) 
    out1 = ((abs(j-(1-k+l)*m)-m+1)*(abs(i-(k+l)*m)-m+1))
else
  out1 = f(mod(i,m),
	   mod(j,m),
	   floor(((bitand(bitxor(i,j),bitxor(bitxor(bitxor(m*l,i)/m),l,k,floor(i/m)))))),
	   floor(((i^j)&((m*k)^i))/m)^l,
	   m/2)
