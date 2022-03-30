r1=p*x + q;
r2 = r1 - (1./r1 - x)./(-1./(r1.^2));
err = abs(r2 - 1./x)./(1./x);
max(err)
