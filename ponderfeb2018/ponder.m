close all;
clear all;
%for n1=1:15;
%for n2=1:n1;
%for n3=1:n2;
n1=4
n2=9
n3=13

2569 / 60;
pn1equalsn = coinCollectorsPMF(n1);
pn2equalsn = coinCollectorsPMF(n2);
pn3equalsn = coinCollectorsPMF(n3);

pn1LessOrEqualsn = cumsum(pn1equalsn);
pn2LessOrEqualsn = cumsum(pn2equalsn);
pn3LessOrEqualsn = cumsum(pn3equalsn);

plot(pn1equalsn)
hold on
plot(pn2equalsn)
plot(pn3equalsn)
plot(pn1LessOrEqualsn)
plot(pn2LessOrEqualsn)
plot(pn3LessOrEqualsn)


ma = max([length(pn1LessOrEqualsn), length(pn2LessOrEqualsn), length(pn3LessOrEqualsn)]);

%ma=40

pn1LessOrEqualsn = [pn1LessOrEqualsn ones(1,ma-length(pn1LessOrEqualsn))];
pn2LessOrEqualsn = [pn2LessOrEqualsn ones(1,ma-length(pn2LessOrEqualsn))];
pn3LessOrEqualsn = [pn3LessOrEqualsn ones(1,ma-length(pn3LessOrEqualsn))];

%pn1LessOrEqualsn = pn1LessOrEqualsn(1:40);
%pn2LessOrEqualsn = pn2LessOrEqualsn(1:40);
%pn3LessOrEqualsn = pn3LessOrEqualsn(1:40);




pMaxLessOrEqualsn = pn1LessOrEqualsn .* pn2LessOrEqualsn .* pn3LessOrEqualsn;

plot(pMaxLessOrEqualsn,'r')

pMaxEqualsn = diff([0 pMaxLessOrEqualsn]);
plot(pMaxEqualsn,'g');

expectMax = sum((1:length(pMaxEqualsn)) .* pMaxEqualsn);

expectMax *60
expectMax *60 - 2569
expectMaxArray(n1,n2,n3) = expectMax *60 - 2569;
%end
%end
%end

solution = (abs(expectMaxArray) < 5) .* (expectMaxArray != 0);


[n1solution,n2solution,n3solution] = ind2sub(size(solution),find(solution))



%%%

%%%
%%%
%%% 6, 7, 13
%%%
%%% 13, 9, 1
%%% 13, 9, 2
%%% 13, 9, 3
%%% 13, 9, 4
