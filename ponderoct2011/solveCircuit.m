for r1=0:20
for r3=0:20-r1
for r4=0:20-r1-r3
for r5=0:20-r1-r3-r4
for r6=0:20-r1-r3-r4-r5
for r7=0:20-r1-r3-r4-r5-r6
if(r1+r3 == 0)
 a=0;
else
a=r1*r3/(r1+r3);
endif
if(a+r4+r5 == 0)
  b=0;
else
b=(a+r4)*r5/(a+r4+r5);
endif
if(b+r6+r7==0)
  c=0;
else
c=(b+r6)*r7/(b+r6+r7);
endif

if (40320==con8prod(a) || 40320 == con8prod(b) || 40320 == con8prod(c))
r1
r3
r4
r5
r6
r7
endif


end
end
end
end
end
end
