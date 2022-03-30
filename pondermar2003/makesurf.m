a=linspace(1,40,30);
b=linspace(-5,-25,50);
for i=1:30
  for j=1:50
    plotarea(i,j)=circles([a(i) b(j)]);
  end
end

surf(a,b,-plotarea');
