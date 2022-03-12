close all;
%clear all;
n1=4
n2=9
n3=13
%for n1=1:15;
%for n2=1:n1;
%for n3=1:n2;

2569 / 60;
pn1equalsnExact = coinCollectorsExactPMF(n1);
pn2equalsnExact = coinCollectorsExactPMF(n2);
pn3equalsnExact = coinCollectorsExactPMF(n3);

pn1LessOrEqualsnExact = cumsum(pn1equalsnExact);
pn2LessOrEqualsnExact = cumsum(pn2equalsnExact);
pn3LessOrEqualsnExact = cumsum(pn3equalsnExact);

plot(pn1equalsnExact)
hold on
plot(pn2equalsnExact)
plot(pn3equalsnExact)
plot(pn1LessOrEqualsnExact)
plot(pn2LessOrEqualsnExact)
plot(pn3LessOrEqualsnExact)


ma = max([length(pn1LessOrEqualsnExact), length(pn2LessOrEqualsnExact), length(pn3LessOrEqualsnExact)]);

%ma=40

%pn1LessOrEqualsnExact = [pn1LessOrEqualsnExact ones(1,ma-length(pn1LessOrEqualsnExact))];
%pn2LessOrEqualsnExact = [pn2LessOrEqualsnExact ones(1,ma-length(pn2LessOrEqualsnExact))];
%pn3LessOrEqualsnExact = [pn3LessOrEqualsnExact ones(1,ma-length(pn3LessOrEqualsnExact))];

%pn1LessOrEqualsnExact = pn1LessOrEqualsnExact(1:40);
%pn2LessOrEqualsnExact = pn2LessOrEqualsnExact(1:40);
%pn3LessOrEqualsnExact = pn3LessOrEqualsnExact(1:40);


pMaxLessOrEqualsnExact = pn1LessOrEqualsnExact .* pn2LessOrEqualsnExact .* pn3LessOrEqualsnExact;

plot(pMaxLessOrEqualsnExact,'r')

pMaxEqualsnExact = diff([0 pMaxLessOrEqualsnExact]);
plot(pMaxEqualsnExact,'g');

expectMaxExact = sum((1:length(pMaxEqualsnExact)) .* pMaxEqualsnExact);

expectMaxExact *60
expectMaxExact *60 - 2569
expectMaxArrayExact(n1,n2,n3) = expectMaxExact *60 - 2569;


%end
%end
%end

solution = (abs(expectMaxArrayExact) < 5) .* (expectMaxArrayExact != 0);


[n1solution,n2solution,n3solution] = ind2sub(size(solution),find(solution))



%%%

%%%
%%%
%%% 6, 7, 13
%%%
%%%