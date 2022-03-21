a=9999;
for k=[1:1000]
  for i=[1:100]
    l1=floor(log10(i))+1;
    l3=floor(log10(k))+1;
    c=k*10000+a;
    b=c*(10^l1)+i;
    l2=floor(log10(b^2))+1;
    for j=[1:l2-1]
      secondhalf=mod(b^2,(10^j));
      firsthalf=(b^2-secondhalf)/10^j;
      firsthalfr=floor(sqrt(firsthalf));
      secondhalfr=floor(sqrt(secondhalf));
      if (secondhalfr^2==secondhalf && firsthalfr^2==firsthalf && secondhalf>=10^(j-1))
        firsthalf
        secondhalf
        b^2
        b
      endif
    endfor
  endfor
endfor
