// Chris Shannon 2011
// April 2011 IBM Ponder This solution program
#include <iostream>
#include <fstream>
#include <sstream>
#include <iomanip>
#include <math.h>
#include <vector>
using namespace std;

class Coordinate
{
public:
  int x;
  int y;
};
//right, up, left, down
int  x[4] = {1, 0, -1, 0};
int  y[4] = {0, 1, 0, -1};
class Velocity
{
public:
  int velocityIndex;
public:
  Velocity()
  {
      
  }
  void turnLeft()
  {
    velocityIndex=(velocityIndex+1)%4;
  }
  void turnRight() 
  {
    velocityIndex=(velocityIndex+3)%4;
  }
};

class Map
{
  // The snake can only reach up to (-10,-10),(-10,10),(10,-10),(10,10),
  // so a 21x21 grid can be indexed
  bool places[511];
public:
  Map()
  {
    for (int i = 0; i< 511; i++)
      {
	places[i]=false;
      }
  }
  void visit(int i, int j)
  {
    places[21*i+j+220] = true;
  }
  bool isVisited(int i, int j)
  {
    return places[21*i+j+220];
  }
};

class Snake
{
public:
  vector<bool> path; // Left is false, right is true, length is number of segments
  Coordinate currentPosition;
  Velocity currentVelocity;
  Map map;
  bool validSnake;
public:
  Snake()
  {
    // First turn is left
    path.push_back(false);
    // It starts by going to the right
    currentVelocity.velocityIndex=1;
    currentPosition.x=1;
    currentPosition.y=1;
    map.visit(0,0);
    map.visit(1,0);
    map.visit(1,1);
    validSnake = true;
  }
    
  bool move(bool turn)
  {
    cout << "Snake with " << path.size() << " turns at (" << currentPosition.x << ","<<currentPosition.y<< ") going ";
    switch(currentVelocity.velocityIndex)
      {
      case(0):
	cout << "right";
	break;
      case(1):
	cout << "up";
	break;
      case(2):
	cout << "left";
	break;
      case(3):
	cout << "down";
	break;
      }
    cout << " turning ";
    path.push_back(turn);
    if (!turn)
      {
	cout << "left"<<endl;
	currentVelocity.turnLeft();
      }
    else
      {
	cout << "right"<<endl;
	currentVelocity.turnRight();
      }
    currentPosition.x += x[currentVelocity.velocityIndex];
    currentPosition.y += y[currentVelocity.velocityIndex];
    if (map.isVisited(currentPosition.x,currentPosition.y))
      {
	// invalid snake
	// Maybe mark this spot on the sierpinski gasket?
	cout << "died at (" << currentPosition.x << ","<<currentPosition.y<< ")"<<endl;
	
	double x=0.5;
	double stepSize = 0.5;
	for(int i = 1; i < path.size(); i++)
	  {
	    stepSize/=2;
	    if (path[i])
	      {
		x+=stepSize;
	      }
	    else
	      {
		x-=stepSize;
	      }
	  }
	cout.precision(15);
	
	cout << "File"<< fixed << path.size() << ","<< x<<endl;
	// grep File octaveIn.txt > octaveIn2.txt
	// emacs octaveIn2.txt
	// octave:9> load octaveIn2.txt

	// octave:13> plot(octaveIn2(:,2),octaveIn2(:,1))
	
	validSnake=false;
	return false;
      }
    map.visit(currentPosition.x,currentPosition.y);
    return true;
  }  
    
  void writeSVG(int fileNumber)
  {
    ofstream myfile;
    string f = "svg";
    ostringstream oss;
    oss << setw(4) << setfill('0') << fileNumber;
    f.append(oss.str());
    f.append(".svg");
      
    myfile.open(&f[0]);
    myfile << "<svg width=\"200px\" height=\"200px\" viewBox=\"-10 -10 20 20\">\n";
      
    myfile << "\n  <polyline points=\"";
    int posx = 0;
    int posy = 0;
    Velocity v;
    v.velocityIndex=0;
    myfile << posx << " " << posy << ", ";
      
    for (int i = 0; i <= path.size(); i++)
      {
	posx += x[v.velocityIndex];
	posy += y[v.velocityIndex];
	myfile << posx << " " << -1*posy;	
	if (i != path.size())
	  {
	    if(path[i])
	      {
		v.turnRight();
	      }
	    else
	      {
		v.turnLeft();
	      }
	    myfile << ", ";
	  }
      }
    myfile << "\"\nstyle=\"stroke: black; stroke-width: .1; fill: none;\"/> \n </svg>";
      
    myfile.close();
      
  }
};

int validSnakeCount = 0;
int numberOfTotalSegments = 19;
void recurse(Snake s)
{
  if (s.validSnake && s.path.size() < numberOfTotalSegments)
    {
      Snake s2 = s;
      if (s2.move(false)) // attempt to move left
	{
	  recurse(s2);
	}
      if (s.move(true)) // attempt to move right
	{
	  recurse(s);
	}
    }
  else if (s.validSnake && s.path.size() == numberOfTotalSegments)
    {
      //print s.path
      cout << "Valid snake, path: ";
      for (int i =0; i < numberOfTotalSegments; i++)
	{
	  cout << " " << i << ":";
	  if( s.path[i]==false)
	    {
	      cout << "L";
	    }
	  else
	    {
	      cout << "R";
	    }
	}
      cout << endl;
    
      s.writeSVG(validSnakeCount);
    
      validSnakeCount++;
    }
}

int main()
{
  Snake s;
  recurse(s);
  cout << "There are " << validSnakeCount << " valid snakes." << endl;
  return 0;
}


/*
  Exhaustive Iterative
  2^18 possibilities = 262144

  for (int i = 0; i < 2^18; i++)
  if check(i)
  {
  validSnakeCount++
  }
*/

