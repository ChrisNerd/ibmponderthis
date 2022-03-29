#include <iostream>
using namespace std;
int m1[]= {0,1,2,3,4,5,
	   0,1,2,4,3,5,
	   0,1,2,5,3,4,
	   0,2,1,3,4,5,
	   0,2,1,4,3,5,
	   0,2,1,5,3,4,
	   0,3,1,2,4,5,
	   0,3,1,4,2,5,
	   0,3,1,5,2,4,
	   0,4,1,2,3,5,
	   0,4,1,3,2,5,
	   0,4,1,5,2,3,
	   0,5,1,2,3,4,
	   0,5,1,3,2,4,
	   0,5,1,4,2,3};

int m2[]={3,4,5,6,7,8,
	  3,4,5,7,6,8,
	  3,4,5,8,6,7,
	  3,5,4,6,7,8,
	  3,5,4,7,6,8,
	  3,5,4,8,6,7,
	  3,6,4,5,7,8,
	  3,6,4,7,5,8,
	  3,6,4,8,5,7,
	  3,7,4,5,6,8,
	  3,7,4,6,5,8,
	  3,7,4,8,5,6,
	  3,8,4,5,6,7,
	  3,8,4,6,5,7,
	  3,8,4,7,5,6};


int m3[]={0,1,2,6,7,8,
	  0,1,2,7,6,8,
	  0,1,2,8,6,7,
	  0,2,1,6,7,8,
	  0,2,1,7,6,8,
	  0,2,1,8,6,7,
	  0,6,1,2,7,8,
	  0,6,1,7,2,8,
	  0,6,1,8,2,7,
	  0,7,1,2,6,8,
	  0,7,1,6,2,8,
	  0,7,1,8,2,6,
	  0,8,1,2,6,7,
	  0,8,1,6,2,7,
	  0,8,1,7,2,6};


void initializeBoard(int* b);
bool gameOn(int* board, int round);
void printBoard(int* board);
void copyBoard(int* board, int* newBoard);
bool fits(int* board, int* m, int i);
void place(int* board, int* m, int i, int round);

int main()
{
  int board[81];
  initializeBoard(board);

  gameOn(board, 0);

}


void initializeBoard(int* b)
{
  for(int i=0; i< 81; i++)
    {
      b[i]=0;
    }
  return;

}

bool gameOn(int* board, int round) 
{
  //  cout << "round " << round <<"\n";
  // printBoard(board);
  if (round>=11)
    {
      printBoard(board);
      return true;
    }
  
  int* m = m1;
  if(round%3==1)
    {
      m=m2;
    }
  else if (round%3 == 2)
    {
      m=m3;
    }

  for (int i=0; i < 15; i++)
    {
      if( fits(board, m, i) )
	{
	  int newBoard[81];
	  copyBoard(board, newBoard);
	  place(newBoard,m,i,round+1);
	  gameOn(newBoard, round+1);
	}
    }
  return false;

}

void printBoard(int* board)
{
  for (int i=0; i<81; i++)
    {
      cout << board[i];
      if(i%9!=8)
	{
	  cout << " ";
	}
      else
	{
	  cout << "\n";
	}
    }
  cout <<"\n";
}
void copyBoard(int* board, int* newBoard)
{
  for (int i=0; i < 81; i++)
    {
      newBoard[i] = board[i];
    }
}

bool fits(int* board, int* m, int i)
{
  int x=m[6*i+0];
  int y=m[6*i+1];
  if (board[9*x+y] != 0)
    return false;
  x=m[6*i+2];
  y=m[6*i+3];
  if (board[9*x+y] != 0)
    return false;
  x=m[6*i+4];
  y=m[6*i+5];
  if (board[9*x+y] != 0)
    return false;
  return true;
}

void place(int* board, int* m, int i, int round)
{
  int x=m[6*i+0];
  int y=m[6*i+1];
  board[9*x+y] = round;
  x=m[6*i+2];
  y=m[6*i+3];
  board[9*x+y] = round;
  x=m[6*i+4];
  y=m[6*i+5];
  board[9*x+y] = round;
  return;
}
