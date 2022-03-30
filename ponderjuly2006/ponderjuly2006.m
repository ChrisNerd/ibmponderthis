%x=linspace(sqrt(2)-1e-2, sqrt(2)+1e-2,1000);
%x = [1 x 2 sqrt(2)];
for choose=1:2
x = linspace(1,2,10000);
r = .1
if (choose==1)
f = 1./x;
elseif (choose ==2)
f = sqrt(x);
end
phat = 0;
qhat = 0;
clear e, phat,qhat;
%figure;
%hold off;
%plot(x,1./x,'b');
hold on;
for z = 1:10
if (z == 1)
  plast = 0;
  qlast = 0;
else
  plast=phat(z-1);
  qlast=qhat(z-1);
end

p = linspace(plast - 100*r^(z), plast + 100*r^(z), 101);
q = linspace(qlast - 100*r^(z), qlast + 100*r^(z), 101);

for i = 1:length(p)
for j = 1:length(q)
     r1=p(i)*x + q(j);
if (choose==1)
     r2 = r1 - (1./r1 - x)./(-1./(r1.^2));
elseif (choose ==2)
     r2 = r1 - (r1.^2 - x)./(2*r1);
end
     err = abs(r2 - f)./f;
     e(i,j)= max(err);
endfor
endfor
[minv mini] = min(e);
[minv2 mini2] = min(minv);
phat(z) = p(mini(mini2));
qhat(z) = q(mini2);
endfor
%mesh(q,p,e);

phat
qhat
minv2
endfor
