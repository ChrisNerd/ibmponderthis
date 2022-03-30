M=3
N=5
k=1:N/2

first=((M-1)/M).^(N-2*k)*M^(N-M)
second=(M-1)^(N-M)
     denom=M*M^(N-M)
     maxfunc=max(first,second)

brute=sum(maxfunc)/denom

e1=((M-1)/M)^(N-M)*1/M*floor(M/2)
e2=1/M*((M-1)/M)^N*(M^2/(2*M-1))*((M/(M-1))^(2*floor(N/2))-(M/(M-1))^(2*floor(M/2)))
elegant=e1+e2



