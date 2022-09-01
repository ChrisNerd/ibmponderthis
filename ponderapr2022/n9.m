function n9(s) = 

% s are the 2nd to 7th elements of x
% the first element of s is -2/3

for aj=1:length(x)
for ai=1:length(x)
dx(ai,aj)= abs(x(aj)-x(ai))
end
end

for aj=1:length(x)
for ai=1:length(x)
dy(ai,aj)= abs(y(aj)-y(ai))
end
end

dd= dx-dy
dd2 = dd.^2
sum(dd2)
sum(sum(dd2))


% seems we have reduced the degrees of freedom down to
% 5 for x. and 0 for y.
% 5 because the first element is set to -2/3, leaving 8.
% We need one degree of freedom to have it sum to 0
% and one degree to have it be length 1.
% and one degree to have the y
% s + a1 + a2 = 0
% s.^2 + a1^2 + a2^2 = 1
% let's solve that
% a2 = -(a1+s)
% s.^2 + a1^2 + (a1+s)^2 = 1
% s.^2 + 2*a1^2 + 2 a1*s +s^2 = 1
% quadratic in a1
% 0 = 2*a1^2 + 2 a1*s + s.^2 + s^2 - 1
% coefficients [2, 2*s, s.^2 + s^2 - 1]
% so if the first coefficents are +-2/3, and if the y's are say m higher than x's each then the sum of the 8 remaining ys is 8m higher than the sum of the 8 remaining xs.
% -2/3 + sum of 8 xs + 8m = 0
% 2/3 + sum of 8 xs = 0
% -4/3 + 8 m =0
% m = 1/6, which is shown with x-y