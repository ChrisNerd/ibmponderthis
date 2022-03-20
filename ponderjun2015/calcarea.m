function parea = calcarea(a)
a=fix(a);
x=real(a);
y=imag(a);

ch=convhull(x,y);
parea = polyarea(x(ch),y(ch));

endfunction
