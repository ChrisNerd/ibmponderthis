p = phat1
q = qhat1
r1=p*x + q;
r2 = r1 - (1./r1 - x)./(-1./(r1.^2));
err = abs(r2 - 1./x)./(1./x);
e1 = max(err)

p= phat1+ 1e-10
q = qhat1 
r1=p*x + q;
r2 = r1 - (1./r1 - x)./(-1./(r1.^2));
err = abs(r2 - 1./x)./(1./x);
max(err) - e1
