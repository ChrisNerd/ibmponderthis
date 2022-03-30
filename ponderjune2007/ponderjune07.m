close all;
n = 0;
i=1;
xlen = 100;
fr = zeros(1,xlen);
nsamp = 1000;
xs = linspace(0,1,xlen);
for x=linspace(0,1,xlen)
	for b=1:nsamp
		a = 0;
		while (a < n + x)
			a = a + rand;
			if (n + x < a && a < n + 1 )
				fr(i) = fr(i) + 1;
			end
		end
	end
	i = i+1;
end
plot(xs,fr / nsamp, ";part 1;");
hold on

n = 20;
i=1;
xlen = 100;
fr2 = zeros(1,xlen);
nsamp = 1000;
xs = linspace(0,1,xlen);
for x=linspace(0,1,xlen)
	for b=1:nsamp
		a = 0;
		while (a < n + x)
			a = a + rand;
			if (n + x < a && a < n + 1 )
				fr2(i) = fr2(i) + 1;
			end
		end
	end
	i = i+1;
end
plot(xs,fr2 / nsamp, ";part 2;");
plot(xs, (1 - xs) .* exp(xs), ";(1-x)e^x;")
plot(xs, (1 - xs) .* (1 + xs),";(1-x)(1+x);")
