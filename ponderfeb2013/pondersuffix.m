a=9999
for i=[1:100000]
        l1=floor(log10(i))+1;
       # print(i)
       # print(l1)
        c=k*10000+a;
        b=c*(10^l1)+i;
        #print(b)
        l2=floor(log10(b^2))+1;
        #print(b^2)
        for j=[1:l2-1]
            secondhalf=b^2%(10^j);
            #print(secondhalf)
            firsthalf=(b^2-secondhalf)/10^j;
            #print(firsthalf)
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
