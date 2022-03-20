function plotcubics(xroots,colour)
for x=1:300
fx(x)=(x-xroots(1)) * (x-xroots(2)) * (x-xroots(3));
endfor
hold on
plot(fx,colour)
endfunction