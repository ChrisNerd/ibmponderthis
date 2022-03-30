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
balls = [0 0 0; # 3/12
1 1 1; # 3/12
0 0 1; # all rest occur with 1/12 probability
0 1 0;
1 0 0;
1 1 0;
1 0 1;
0 1 1;];
strategyresultr = zeros(26,8);
strategyresultc = zeros(26,8);
multfactor = [ 3 3 1 1 1 1 1 1]' / 12;
for trial = 1:8
	b1 =balls(trial,1);
	b2= balls(trial,2);
	b3= balls(trial,3);
	for i=1:26
		ct = 0; # total cost for this strategy
		r = 0; # revenue for this strategy
		p = 0; # profit for this strategy
		c = 1;
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
		strategyresultr(i,trial) = r ; # expected revenue for this strategy and this ball structure
		strategyresultc(i,trial) = ct; # expected number of  for this strategy and this ball structure
	endfor # for i=1:26
endfor # for trial = 1:8

numc = 101;
costs = linspace(0,1,numc);
for i = 1:numc
	c = costs(i);
	[opti(i) ind] = max((strategyresultr - c* strategyresultc )* multfactor) ;
	ind
	allstrats(:,i) = (strategyresultr - c* strategyresultc )* multfactor;
endfor
plot(costs, opti);
plot(costs, allstrats);
