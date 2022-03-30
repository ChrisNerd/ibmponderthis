max = 512;
for x=1:max
count(x) = 0;
for a256=0:2
for a128=0:2
for a64=0:2
for a32=0:2
for a16=0:2
for a8=0:2
for a4=0:2
for a2=0:2
for a1=0:2
if (
a256 * 2^8 +
a128 * 2^7 +
a64 * 2^6 +
a32 * 2^5 +
a16 * 2^4 +
a8 * 2^3 +
a4 * 2^2 +
a2 * 2^1 +
a1 * 2^0
== x )
     count(x) = count(x) + 1;
end
end
end
end
end
end
end
end
end
end
end
plot(2:max,count(2:max)./count(1:max-1))
