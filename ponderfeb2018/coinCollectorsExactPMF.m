function pNequalsmExact = coinCollectorsExactPMF(n)

% P[T=m]=1/n^m×n×{m−1n−1}×(n−1)!
% https://math.stackexchange.com/questions/1609459/coupon-collectors-problem-using-inclusion-exclusion

%P[T=m]=1/n^m×n×{m−1n−1}×(n−1)!

for m=1:1000
pNequalsmExact(m) =1/n^m * n* nstir2k(m-1,n-1) * factorial(n-1);
end

