bmat = zeros(512,9);

for i=1:512
im = i-1;
for j=1:9
bmat(i,10-j)=mod(im,2);
im=floor(im/2);
endfor
endfor
cmat=bmat;


score=zeros(512,512);
for i=1:512
 c=cmat(i,:);
 for j=1:512
  b=bmat(j,:);
  a=zeros(1,9);
  a(1)=1;
  s=0;
  for k=2:9
%   s=s+a(k-1)+b(k-1)+c(k-1);
   s=s+b(k-1)+c(k-1);
   a(k)=mod(s,2);
  endfor
  for k=1:9
   if (a(k)==b(k) && a(k) ==c(k))
    score(i,j)++;
   endif
  endfor
 endfor
endfor
s1=max(score,[],2); % max of the row, should result in 512x1
s2 = min(s1)
plot(s1)
