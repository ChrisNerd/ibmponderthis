setofsquares=10:219;
setofsquares=setofsquares.^2
for a=100:12000
b=setofsquares - a;
b=b( b>a );


#b=a+1:12001;
#t=ismember((a+b),setofsquares);
#b=b(t);
#a
#length(b)
for i=1:length(b)
  for j=(i+1):length(b)
   if (ismember(b(i)+b(j),setofsquares))
     for k=(j+1):length(b)
   if(ismember(a+b(i)+b(j)+b(k),setofsquares) && ...
       ismember(b(i)+b(k),setofsquares) &&...
      ismember(b(j)+b(k),setofsquares))
     wow1=b(i)
     wow2=b(j)
     wow3=b(k)
   end
 end
end
end
end
end

