a=9999
for i in range(1,10000):
    for k in range(1,100000):
        l1=floor(log(i,10))+1;
        l3=floor(log(k,10))+1;
       # print(i)
       # print(l1)
        c=k*10000+a
        b=c*(10^l1)+i;
        #print(b)
        l2=floor(log(b^2,10))+1;
        j=1
        #print(b^2)
        for j in range(1,l2):
            secondhalf=b^2%(10^j)
            #print(secondhalf)
            firsthalf=(b^2-secondhalf)/10^j
            #print(firsthalf)
            firsthalfr=floor(sqrt(firsthalf))
            secondhalfr=floor(sqrt(secondhalf))
            if secondhalfr^2==secondhalf and firsthalfr^2==firsthalf and secondhalf>=10^(j-1):
                print(firsthalf)
                print(secondhalf)
                print(b^2)
                print(b)
