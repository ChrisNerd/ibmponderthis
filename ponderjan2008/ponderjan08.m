strategy = [0 0 0 0 0 0 0; # don't buy the first
1 0 0 0 0 0 0; # buy the first, don't buy the second
1 1 0 0 0 0 0; # buy the first, buy the second if it is black
1 0 1 0 0 0 0; # buy the first, buy the second if it is white
1 1 1 0 0 0 0; # buy the first and second regardless but not the third
1 1 0 1 0 0 0;
1 1 0 0 1 0 0;
1 1 0 1 1 0 0; #--end case of first one being black
1 0 1 0 0 0 1;
1 0 1 0 0 1 0;
1 0 1 0 0 1 1; #-- end case of the first one being white
1 1 1 0 0 0 1; # begin buy the second regardless, but the third on conditions on the first two... 16 cases
1 1 1 0 0 1 0;
1 1 1 0 0 1 1;
1 1 1 0 1 0 0;
1 1 1 0 1 0 1;
1 1 1 0 1 1 0;
1 1 1 0 1 1 1;
1 1 1 1 0 0 0;
1 1 1 1 0 0 1;
1 1 1 1 0 1 0;
1 1 1 1 0 1 1;
1 1 1 1 1 0 0;
1 1 1 1 1 0 1;
1 1 1 1 1 1 0;
1 1 1 1 1 1 1;];
ncosts = 101;
costs = linspace(0, .7, ncosts);
cresults = zeros(1, ncosts);
for ci = 1:ncosts
	c = costs(ci); # unit cost
	strategyresult = zeros(26,1);
	numTrials = 5000;
	#[ first secondiffirst_black second_if_first_white third_if_bb third_if_bw third_if_wb third_if_ww]
	#[ 1       2                           3                       4               5             6                 7 ];
	for i=1:26
		ct = 0; # total cost for this strategy
		r = 0; # revenue for this strategy
		p = 0; # profit for this strategy
		for trial = 1:numTrials
			n = floor(4*rand()); # number of white balls present 0, 1, 2 or 3
			place=-1;
			if (n != 0 && n != 3)
				# the placement of the odd one out, possible values are 1, 2 or 3
				place =1+floor(3* rand());
			endif
			if (n == 0)
				b1 = 0;
				b2 = 0;
				b3 = 0;
			endif
			if (n == 1 && place == 1)
				b1 = 1;
				b2 = 0;
				b3 = 0;
			endif
			if (n == 1 && place == 2)
				b1 = 0;
				b2 = 1;
				b3 = 0;
			endif
			if (n== 1 && place == 3)
				b1 = 0;
				b2 = 0;
				b3 = 1;
			endif

			if (n==2 && place == 1)
				b1 = 0;
				b2 = 1;
				b3 = 1;
			endif
			if (n==2 && place == 2)
				b1 = 1;
				b2 = 0;
				b3 = 1;
			endif
			if (n==2 && place == 3)
				b1 = 1;
				b2 = 1;
				b3 = 0;
			endif

			if (n==3)
				b1 =1;
				b2= 1;
				b3= 1;
			endif


			if (strategy(i,1) != 0) # do we buy the first ball?
				ct = ct + c; # just bought one ball
				if (b1 == 0)
					# first ball was black
					if (strategy(i,2) != 0) # do we buy the second ball given the first was black?
						ct = ct + c; # just ball a second ball
						if (b2 == 0)
							#second ball was black
							if (strategy(i,4) != 0) # do we buy the third ball given we have bb?
								ct = ct + c; # just ball a third ball
								if (b3 == 0)
									#third ball was black
								else
									#third ball was white
									r = r + 1;
								endif
							endif
						else
							#second ball was white
							r = r + 1;
							if (strategy(i,5) != 0) # do we buy the third ball given we have bw?
								ct = ct + c; # just ball a third ball
								if (b3 == 0)
									#third ball was black
								else
									#third ball was white
									r = r + 1;
								endif
							endif
						endif
					endif
				else
					# first ball was white
					r = r + 1;
					if (strategy(i,3) != 0) # do we buy the second ball give the first was white?
						ct = ct + c; # just ball a second ball
						if (b2 == 0)
						#second ball was black
							if (strategy(i,6) != 0) # do we buy the third ball given we have wb?
								ct = ct + c; # just ball a third ball
								if (b3 == 0)
									#third ball was black
								else
									#third ball was white
									r = r + 1;
								endif
							endif
						else
							#second ball was white
							r = r + 1;
							if (strategy(i,7) != 0) # do we buy the third ball given we have ww?
								ct = ct + c; # just ball a third ball
								if (b3 == 0)
									#third ball was black
								else
									#third ball was white
									r = r + 1;
								endif
							endif
						endif
					endif
				endif
			endif
		endfor # for trial = 1:numTrials
		strategyresult(i,1) = (r - ct) / numTrials;
	endfor # for i = 1:26
	cresults(ci) = max(strategyresult);
endfor # for ci = 1:ncosts
plot(costs, cresults);