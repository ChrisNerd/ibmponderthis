/*
  Identify each position as winning or losing using induction
  Identify all positions when you will win in 1 move.
  From there, identify all positions that are 1 move away from those and are not already in the winning in 1 set.  Those are all losing positions.  Call those Losing in 2.
  Identify all moves 1 move away from Losing in 2 that are not in Losing in 2.  Call those winning in 3.
  Repeat until [1, 2, 3, 5, 8, 13] is found.

  Alternatively use a Monty Carlo Tree Search.  This might not solve the problem as stated, itstead it would give a best guess as to the correct move but would probably be beaten by anyone who understands the math of the game.  See Nim.

  Winning in 1 are all configurations that have only 1 face remaining.
  [a,b,0,0,0,c]
  [a,0,b,c,0,0]
  [0,a,0,b,c,0]
  [0,0,a,0,b,c]
  where at least one of a,b or c are nonzero. and each is less than [1,2,3,5,8,13]
  Faces are
  1,2,13... 2*3*14-1=83
  1,3,5... 2*4*6-1=47
  2,5,8... 3*6*9-1=161
  3,8,13... 4*9*14-1=503

  83+47+161+503 = 794

  Fibinacci numbers
  00000,1
  00000,2
  00000,3
  00000,4
  ...
  00000,9
  00000,10
  00000,11
  00000,12
  00000,13
  00001,0

Opposite edges
0-4
1-2
3-5

Loser if opposite edges are equal and all others are 0.
Loser if all pairs of opposite edges are 0,1.
Loser if all pairs of opposite edges differ by 1, (generalizes previous line).
Loser if all pairs of opposite edges differ by the same amount.  (generalizes everything so far).
ahh, previous line is wrong,
correct line is Loser if all pairs of opposite edges differ with the same parity (all even, or all odd).

  There are 794 winning positions at level 1.

  There are also at most 794 possible moves at any time.

  =2*3*4*6*9*14=18144 total possible positions including the end game 000000

  Create two 18144 binary array.  isWinning and isLosing.
*/

#include<vector>
#include<iostream>
using namespace std;
typedef vector<int> State;

int indexFromState( State list)
{
  return  3*4*6*9*14*list[0]+
    4*6*9*14*list[1]+
    6*9*14*list[2]+
    9*14*list[3]+
    14*list[4]+
    list[5];
}


State stateFromIndex( int index)
{
  int i= index;
  State s(6);
  s[0] = index/(3*4*6*9*14);
  index -= s[0]*3*4*6*9*14;
  s[1] = index/(4*6*9*14);
  index -= s[1]*4*6*9*14;
  s[2] = index/(6*9*14);
  index -= s[2]*6*9*14;
  s[3] = index/(9*14);
  index -= s[3]*9*14;;
  s[4] = index/(14);
  index -= s[4]*14;
  s[5] = index;
  //  cout << s[0] << " " << s[1] << " " << s[2] << " " << s[3] << " " << s[4] << " " << s[5] << " " << i << endl;
  return s;
}

bool areThereUnknowns(vector<bool>& winners, vector<bool>& losers)
{
  for (int i =0 ; i < winners.size(); i++)
    {
      if (!winners[i] && !losers[i])
	{
	  State s = stateFromIndex(i);
	  cout << "Lowest unknown " << i << " " << s[0]<< " " <<s[1]<< " "<<s[2]<< " " <<s[3]<< " " <<s[4]<< " " <<s[5]<<endl;
	  return true;
	}
    }
  return false;
}
vector<State> movesLeadingToState(State s)
{
  vector<State> r;
  //[a,b,0,0,0,c] 0,1,5
  //[a,0,b,c,0,0] 0,2,3
  //[0,a,0,b,c,0] 1,3,4
  //[0,0,a,0,b,c] 2,4,5
  //[1, 2, 3, 5, 8, 13]

  for(int a=s[0]; a <= 1; a++)
    {
      for(int b=s[1]; b <= 2; b++)
	{
	  for(int c=s[5]; c <= 13; c++)
	    {
	      if (a==s[0] && b==s[1] && c==s[5])
		{
		  continue;
		}
	      int arr[6]= {a,b,s[2],s[3],s[4],c};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }

  for(int a=s[0]; a <= 1; a++)
    {
      for(int b=s[2]; b <= 3; b++)
	{
	  for(int c=s[3]; c <= 5; c++)
	    {
	      if (a==s[0] && b==s[2] && c==s[3])
		{
		  continue;
		}
	      
	      int arr[6]= {a,s[1],b,c,s[4],s[5]};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }

  for(int a=s[1]; a <= 2; a++)
    {
      for(int b=s[3]; b <= 5; b++)
	{
	  for(int c=s[4]; c <= 8; c++)
	    {
	      if (a==s[1] && b==s[3] && c==s[4])
		{
		  continue;
		}
	      int arr[6]= {s[0],a,s[2],b,c,s[5]};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }


  for(int a=s[2]; a <= 3; a++)
    {
      for(int b=s[4]; b <= 8; b++)
	{
	  for(int c=s[5]; c <= 13; c++)
	    {
	      if (a==s[2] && b==s[4] && c==s[5])
		{
		  continue;
		}
	      int arr[6]= {s[0],s[1],a,s[3],b,c};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }
  return r;
}

vector<State> movesFromState(State s)
{
  vector<State> r;
  //[a,b,0,0,0,c] 0,1,5
  //[a,0,b,c,0,0] 0,2,3
  //[0,a,0,b,c,0] 1,3,4
  //[0,0,a,0,b,c] 2,4,5
  for(int a=0; a <= s[0]; a++)
    {
      for(int b=0; b <= s[1]; b++)
	{
	  for(int c=0; c <= s[5]; c++)
	    {
	      // must take at least 1
	      if (a==s[0] && b==s[1] && c==s[5])
		{
		  continue;
		}
	      int arr[6]= {a,b,s[2],s[3],s[4],c};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }

  for(int a=0; a <= s[0]; a++)
    {
      for(int b=0; b <= s[2]; b++)
	{
	  for(int c=0; c <= s[3]; c++)
	    {
	      // must take at least 1
	      if (a==s[0] && b==s[2] && c==s[3])
		{
		  continue;
		}
	      int arr[6]= {a,s[1],b,c,s[4],s[5]};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }

  for(int a=0; a <= s[1]; a++)
    {
      for(int b=0; b <= s[3]; b++)
	{
	  for(int c=0; c <= s[4]; c++)
	    {
	      // must take at least 1
	      if (a==s[1] && b==s[3] && c==s[4])
		{
		  continue;
		}
	      int arr[6]= {s[0],a,s[2],b,c,s[5]};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }

  for(int a=0; a <= s[2]; a++)
    {
      for(int b=0; b <= s[4]; b++)
	{
	  for(int c=0; c <= s[5]; c++)
	    {
	      // must take at least 1
	      if (a==s[2] && b==s[4] && c==s[5])
		{
		  continue;
		}
	      int arr[6]= {s[0],s[1],a,s[3],b,c};
	      r.push_back( State(arr,arr+6));
	    }
	}
    }
  return r;
}

void findAllWinners(vector<bool>& winners, vector<bool>& losers)
{
  // a winner is a state that is one move before a loser
  // So filter for all the currently identified losers
  //    foreach loser in losers
  for (int i=0; i < losers.size(); i++)
    {
      if(!losers[i])
	{
	  continue;
	}
      //      cout << "here1" << endl;
      vector<State> winnersToAdd = movesLeadingToState(stateFromIndex(i));
      //      cout << "here2" << endl;
      for (int j=0; j < winnersToAdd.size(); j++)
	{
	  winners[indexFromState(winnersToAdd[j])]=true;
	}
    }
}

vector<State> findAllPotentialLosers(vector<bool>& winners, vector<bool>& losers)
{
  // a potential loser is one that is
  // a) one move from a winner
  // b) not already identified as a winner
  // c) not already identified as a loser
  vector<State> s;
  for (int i =0; i < winners.size(); i++)
    {
      if (!winners[i])
	{
	  continue;
	}
      vector<State> potentialLosersToAdd = movesLeadingToState(stateFromIndex(i));
      for (int j=0; j < potentialLosersToAdd.size(); j++)
	{
	  int ind = indexFromState(potentialLosersToAdd[j]);
	  if (!winners[ind] && !losers[ind])
	    {
	      s.push_back(potentialLosersToAdd[j]);
	    }
	}
    }
  return s;      
}

bool allMovesWinner(vector<bool>& winners, vector<State>& movesAvailabletoPotentialLoser)
{
  //  foreach move in movesAvailabletoPotentialLoser
  for (int i =0; i <  movesAvailabletoPotentialLoser.size(); i++)
    {
      if (!winners[indexFromState(movesAvailabletoPotentialLoser[i])])
	{
	  return false;
	}
    }
  return true;

}

void filterForRealLosers(vector<bool>& winners, 
				  vector<bool>& losers, 
				  vector<State>& potentialLosers)
{
  // the only moves available to a real losers are winners
  // if at least one move available to a potential loser is not identified as a winner,
  // then it is not a loser
  vector<State> realLosers;
  //  for each pl in potentialLosers
  for (int i =0; i < potentialLosers.size();i++)
    {
      vector<State> movesAvailabletoPotentialLoser = movesFromState(potentialLosers[i]);
      // Could probably combine movesFromState and allMovesWinner to short circuit
      // the "all" operation for efficiency
      if (allMovesWinner(winners, movesAvailabletoPotentialLoser))
	{
	  // We found a true loser
	  realLosers.push_back(potentialLosers[i]);
	}
    }
  for (int j=0; j < realLosers.size(); j++)
    {
      losers[indexFromState(realLosers[j])]=true;
    }
}


int main()
{
  vector<bool> losers(2*3*4*6*9*14);
  // start with state at index 0 being a loser
  // This is the equivalent of the winner handing the game, once it's over
  // to the loser.
  losers[0]=true;
  vector<bool> winners(2*3*4*6*9*14);
  int round =0;
    while (areThereUnknowns(winners,losers))
    {
      round++;
      cout << "round"<< round<< endl;
      findAllWinners(winners,losers);
	
      vector<State> pl=findAllPotentialLosers(winners,losers);
      //from the potential losers, filter for real losers
      filterForRealLosers(winners,losers,pl);
      }
  
  for(int i = 0; i < winners.size(); i++)
    {
      if (winners[i] && losers[i])
	cout << "problem at " << i << endl;
      cout << i << " ";
      if (winners[i])
	cout << 1;
      else
	cout << 0;
      cout << "\n";
    }

  // List of loser moves
  for (int i=0; i < losers.size(); i++)
    {
      if (losers[i])
	{
	  State l = stateFromIndex(i);
	  cout << "loser " << i << " " << l[0] << " " 
	       << l[1] << " " 
	       << l[2] << " "
	       << l[3] << " "
	       << l[4] << " "
	       << l[5] << "f";
	  // Testing lemma
	  // Loser if all pairs of opposite edges differ with the same parity (all even, or all odd).
	  // Pairs are (0,4)(1,2) and (3,5).
	  // and all faces have to sum to the same parity
	  int diff1 = l[0]-l[4];
	  int diff2 = l[1]-l[2];
	  int diff3 = l[3]-l[5];
	  if (!(diff1%2 == 0 && diff2%2 == 0 && diff3%2 ==0 ||
		diff1%2 != 0 && diff2%2 != 0 && diff3%2 !=0))
	    cout << "Loser inviolates theorem 1" << endl;

  //[a,b,0,0,0,c] 0,1,5
  //[a,0,b,c,0,0] 0,2,3
  //[0,a,0,b,c,0] 1,3,4
  //[0,0,a,0,b,c] 2,4,5
	  //test the face sums parity
	  int facesum1 = l[0]+l[1]+l[5];
	  int facesum2 = l[0]+l[2]+l[3];
	  int facesum3 = l[1]+l[3]+l[4];
	  int facesum4 = l[2]+l[4]+l[5];
	  cout << facesum1 << " " << facesum2 << " " << facesum3 << " " << facesum4 << "v";
	  if (!(facesum1%2==0 && facesum2%2==0 && facesum3%2==0 && facesum4%2==0 ||
		facesum1%2!=0 && facesum2%2!=0 && facesum3%2!=0 && facesum4%2!=0))
	    cout << "Loser inviolates theorem 2" << endl;

      // Try the vertex sums
      int vertsum1 = l[0]+ l[1]+ l[3];
      int vertsum2 = l[0]+ l[2]+ l[5];
      int vertsum3 = l[1]+ l[4]+ l[5];
      int vertsum4 = l[2]+ l[3]+ l[4];
      cout << vertsum1 << " " << vertsum2 << " " << vertsum3 << " " << vertsum4 << endl;
    }
      // Need more theorems, something to explains why l[5] is always less than or equal to 8.
      // l[5] ==8 only when 
      // 8 requires 4 bits... 1000

      // Wait, all all face nim-sums zero?
      // 1+2+8 = 1 + 10 + 1000 = 1011
      // 1+1+5 = 101
      // no!
      // The highest face needs to be summable by at least 1 subset of the other 3

      // l[4] only 1 times goes above 3 at 
      // loser 11774 1 0 3 3 4 0
      // so it seems that opposite pairs can only differ by at most 3.

      //loser 16542 1 2 1 5 2 8.
      // l[5] == 7 
      //loser 6713  0 2 0 5 2 7
      //loser 15799 1 2 0 5 3 7 
      //loser 16415 1 2 1 4 2 7 
      
      // Trinary nim sum at any vertex == 0??!!

      // vertex 1 1 1, = 0
      // vertex 2 2 2, = 0
      // vertex 3 3 3, = 0
      // that's the "biggest" vertex (with 3 as the largest minimum)

      // Lets try to generate the list.
    }
  vector<bool> generatedLoserStates(2*3*4*6*9*14);

  for(int a=0; a <= 1; a++)
    {
      for(int b=0; b <= 2; b++)
	{
	  for(int c=0; c <= 3; c++)
	    {
	      for(int d=0; d <= 5; d++)
		{
		  for(int e=0; e <= 8; e++)
		    {
		      for(int f=0; f <= 13; f++)
			{
			  int diff1 = a-e;
			  int diff2 = b-c;
			  int diff3 = d-f;
			  if (!(diff1%2 == 0 && diff2%2 == 0 && diff3%2 ==0 ||
				diff1%2 != 0 && diff2%2 != 0 && diff3%2 !=0)
			      || diff1*diff1 >9 || diff2*diff2 >9 ||diff3*diff3 >9 )
			    {
			      continue;
			    }
			  int arr[6]= {a,b,c,d,e,f};
			  generatedLoserStates[indexFromState( State(arr,arr+6))] = true;
			  cout << "generated State " << a << " " << b << " " << c << " " << d << " " << e << " " << f << endl;
			}
		    }
		}
	    }
	}
    }
  bool areWeRight = losers==generatedLoserStates;
  cout << areWeRight << endl;
  
  // List winning moves from 1,2,3,5,8,13
  int arrFinal[6]= {1,2,3,5,8,13};
  State sFinal(arrFinal,arrFinal+6);
  vector<State> possibleMoves = movesFromState(sFinal);
  // filter for the losing moves
  for(int i =0; i < possibleMoves.size(); i++)
    {
      if (losers[ indexFromState(possibleMoves[i])])
	cout << possibleMoves[i][0] << " " 
	     << possibleMoves[i][1] << " " 
	     << possibleMoves[i][2] << " "
	     << possibleMoves[i][3] << " "
	     << possibleMoves[i][4] << " "
	     << possibleMoves[i][5] << endl;
      
    }
  return 0;

  // Result is 
  /*loser 0 0 0 0 0 0 0
loser 127 0 0 0 1 0 1
loser 254 0 0 0 2 0 2
loser 381 0 0 0 3 0 3
loser 508 0 0 0 4 0 4
loser 635 0 0 0 5 0 5
loser 896 0 0 1 1 1 0
loser 1023 0 0 1 2 1 1
loser 1150 0 0 1 3 1 2
loser 1277 0 0 1 4 1 3
loser 1404 0 0 1 5 1 4
loser 1792 0 0 2 2 2 0
loser 1919 0 0 2 3 2 1
loser 2046 0 0 2 4 2 2
loser 2173 0 0 2 5 2 3
loser 2688 0 0 3 3 3 0
loser 2815 0 0 3 4 3 1
loser 2942 0 0 3 5 3 2
loser 3039 0 1 0 0 1 1
loser 3166 0 1 0 1 1 2
loser 3293 0 1 0 2 1 3
loser 3420 0 1 0 3 1 4
loser 3547 0 1 0 4 1 5
loser 3674 0 1 0 5 1 6
loser 3780 0 1 1 0 0 0 same as the start
loser 3907 0 1 1 1 0 1
loser 4034 0 1 1 2 0 2
loser 4161 0 1 1 3 0 3
loser 4288 0 1 1 4 0 4
loser 4415 0 1 1 5 0 5
loser 4676 0 1 2 1 1 0
loser 4831 0 1 2 2 3 1
loser 4958 0 1 2 3 3 2
loser 5085 0 1 2 4 3 3
loser 5212 0 1 2 5 3 4
loser 5572 0 1 3 2 2 0
loser 5699 0 1 3 3 2 1
loser 5826 0 1 3 4 2 2
loser 5953 0 1 3 5 2 3
loser 6078 0 2 0 0 2 2
loser 6205 0 2 0 1 2 3
loser 6332 0 2 0 2 2 4
loser 6459 0 2 0 3 2 5
loser 6586 0 2 0 4 2 6
loser 6713 0 2 0 5 2 7
loser 6819 0 2 1 0 1 1
loser 6974 0 2 1 1 3 2
loser 7101 0 2 1 2 3 3
loser 7228 0 2 1 3 3 4
loser 7355 0 2 1 4 3 5
loser 7482 0 2 1 5 3 6
loser 7560 0 2 2 0 0 0 same as start
loser 7687 0 2 2 1 0 1
loser 7814 0 2 2 2 0 2
loser 7941 0 2 2 3 0 3
loser 8068 0 2 2 4 0 4
loser 8195 0 2 2 5 0 5
loser 8456 0 2 3 1 1 0 keeps going
loser 8611 0 2 3 2 3 1
loser 8710 0 2 3 3 1 2
loser 8837 0 2 3 4 1 3
loser 8964 0 2 3 5 1 4
loser 9086 1 0 0 0 1 0
loser 9213 1 0 0 1 1 1
loser 9340 1 0 0 2 1 2
loser 9467 1 0 0 3 1 3
loser 9594 1 0 0 4 1 4
loser 9721 1 0 0 5 1 5
loser 9829 1 0 1 0 0 1
loser 9956 1 0 1 1 0 2
loser 9982 1 0 1 1 2 0
loser 10083 1 0 1 2 0 3
loser 10210 1 0 1 3 0 4
loser 10337 1 0 1 4 0 5
loser 10464 1 0 1 5 0 6
loser 10878 1 0 2 2 3 0
loser 11005 1 0 2 3 3 1
loser 11132 1 0 2 4 3 2
loser 11259 1 0 2 5 3 3
loser 11621 1 0 3 2 2 1
loser 11748 1 0 3 3 2 2
loser 11774 1 0 3 3 4 0
loser 11875 1 0 3 4 2 3
loser 12002 1 0 3 5 2 4
loser 12125 1 1 0 0 2 1
loser 12222 1 1 0 1 0 0
loser 12349 1 1 0 2 0 1
loser 12476 1 1 0 3 0 2
loser 12603 1 1 0 4 0 3
loser 12730 1 1 0 5 0 4
loser 12866 1 1 1 0 1 0
loser 13609 1 1 2 0 0 1
loser 14014 1 1 2 3 2 0
loser 14268 1 1 2 5 2 2
loser 14658 1 1 3 2 3 0
loser 15164 1 2 0 0 3 2
loser 15291 1 2 0 1 3 3
loser 15418 1 2 0 2 3 4
loser 15545 1 2 0 3 3 5
loser 15672 1 2 0 4 3 6
loser 15799 1 2 0 5 3 7
loser 15907 1 2 1 0 2 3
loser 16002 1 2 1 1 0 0
loser 16161 1 2 1 2 2 5
loser 16288 1 2 1 3 2 6
loser 16415 1 2 1 4 2 7
loser 16542 1 2 1 5 2 8
loser 16646 1 2 2 0 1 0
loser 16900 1 2 2 2 1 2
loser 17027 1 2 2 3 1 3
loser 17154 1 2 2 4 1 4
loser 17281 1 2 2 5 1 5
loser 17389 1 2 3 0 0 1
loser 17643 1 2 3 2 0 3
loser 17770 1 2 3 3 0 4
loser 17794 1 2 3 3 2 0
loser 17897 1 2 3 4 0 5
loser 18024 1 2 3 5 0 6
  //[a,b,0,0,0,c] 0,1,5
  //[a,0,b,c,0,0] 0,2,3
  //[0,a,0,b,c,0] 1,3,4
  //[0,0,a,0,b,c] 2,4,5

// The first 4 uniquely determine the last 2
*/
  // 1 2 0 5 3 7 so took away 0 0 3 0 5 6  face sums are 10,6,10,10.  Nim sum of the face sums. 1010 + 110 = 1100
  // 1 2 1 5 2 8 so took away 0 0 2 0 6 5  face sums are 11,7,9,11
  // 1 2 2 5 1 5 so took away 0 0 1 0 7 8  8 8 8 8
  // 1 2 3 5 0 6 so took away 0 0 0 0 8 7  9 9 7 9
}
