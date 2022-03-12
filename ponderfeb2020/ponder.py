import matplotlib.pyplot as plt
import random, math, pylab

#https://jakevdp.github.io/blog/2017/12/18/simulating-chutes-and-ladders/
#In the famous Snakes and Ladders game, there is a board with 100 squares. You start at square 0 (just outside of the board) and then proceed to advance based on your move, which is dictacted by the throw of a fair dice. If your piece lands on a square that is at the very bottom of a ladder, you climb it. If your turn lands you at the head of a snake, you slide down to the bottom of its tail. And if your turn takes you out of the board (a square > 100), you stay in the square where you are at the time the dice was thrown.

#Your challenge this month is to design a game with 10 ladders and snakes altogether that will lead to an expected number of moves (rounded to the 6th decimal place) of 66.978705.

#Provide your answer as a list of 10 [source,target] pairs.

#As an example, the standard game of 19, with 9 ladders and 10 snakes:
#[1,38],[4,14],[9,31],[21,42],[28,84],[36,44],[51,67],[71,91],[80,100],[16,6],[47,26],[49,11],[56,53],[62,19],[64,60,],[87,24],[93,73],[95,75],[98,78] has an expected 39.225122 moves (again, rounded to the 6th decimal place).




#Bonus '*' for getting to at least 12 digits after the decimal point to #66.978705007555420778.



plt.style.use('seaborn')
import numpy as np
# Mapping of start : end spaces of chutes & ladders
CHUTES_LADDERS = {1:38, 4:14, 9:31, 16:6, 21:42, 28:84, 36:44,
                  47:26, 95:75, 98:78}

#CHUTES_LADDERS = {99: 9, 2: 66, 11: 44, 35: 33, 7: 37, 87: 40, 5: 21, 42: 43, 4: 9, #68: 55}

#QGuiApplication::setAttribute(Qt::AA_EnableHighDpiScaling, true);
print('Hello World!')
def cl_markov_matrix(max_roll=6, jump_at_end=True):
    """
    Create a Markov transition matrix
    
    If jump_at_end is True, then apply ladder/chute jumps at the end of each turn.
    If False, then apply them at the beginning of the next turn.
    """  
    # Create the basic transition matrix:
    mat = np.zeros((101, 101))
    for i in range(101):
        mat[i + 1:i + 1 + max_roll, i] = 1. / max_roll
        
    # We could alternatively use scipy.linalg.circulent as follows:
    # mat = circulant([0, *np.ones(max_rolls) / 6, *np.zeros(100)])[:101, :101]

    # rolls off the end of the board don't change the state;
    # add these probabilities to the diagonal
    mat[range(101), range(101)] += 1 - mat.sum(0)

    # account for the presence of chutes and ladders
    # we'll do this via  another transition matrix
    cl_mat = np.zeros((101, 101))
    ind = [CHUTES_LADDERS.get(i, i) for i in range(101)]
    cl_mat[ind, range(101)] = 1
    if jump_at_end:
        return cl_mat @ mat
    else:
        return mat @ cl_mat
print('3')
mat = cl_markov_matrix()
print('4')
plt.matshow(mat)
print('5')
plt.grid(False)
print('6')
plt.show()

def expected_moves_from_start( M ):
#    inv(eye(101) - M) @ ones(101)[0]
    N = np.linalg.inv(np.eye(100) - M[:100, :100])

    expected_visits = N[:, 0]
    #plt.plot(expected_visits)
    #plt.xlabel('Square Number')
    #plt.ylabel('Expected Number of Visits in a Game');

    np.set_printoptions(precision=1)
    return np.dot(N.T, np.ones(100))[0]


print(expected_moves_from_start( cl_markov_matrix()))

beta = 1.0
n_accept = 0
best_energy = float('inf')
def energy_function():
    return abs(expected_moves_from_start( cl_markov_matrix()) - 66.978705007555420778)
energy = energy_function()
while best_energy > pow(10,-7):
    if n_accept == 100:
        beta *= 1.005
        n_accept = 0
        print(beta)
        print(best_energy)
        print(best_CHUTES_LADDERS)
        print(expected_moves_from_start( cl_markov_matrix()))
    
    old_CHUTES_LADDERS = CHUTES_LADDERS.copy()
    p = random.uniform(0.0, 1.0)
    s, d = random.choice(list(CHUTES_LADDERS.items()))
    if p < 0.25:
        s2=(s-1+100) % 100
        if not s2 in CHUTES_LADDERS:
            del CHUTES_LADDERS[s]
            CHUTES_LADDERS[s2] = d
    elif p < 0.5:
        s2=(s+1) % 100
        if not s2 in CHUTES_LADDERS:
            del CHUTES_LADDERS[s]
            CHUTES_LADDERS[s2] = d        
    elif p < 0.75:
        CHUTES_LADDERS[s] = (d-1+100) % 100
    else:
        CHUTES_LADDERS[s] = (d+1) % 100
#    new_mat = 
    
    new_energy = abs(expected_moves_from_start(cl_markov_matrix()) - 66.978705007555420778)
#    print(new_energy)
#    print(CHUTES_LADDERS)
    if new_energy < pow(10,-7):
        print("Solution FOUND!!!")
        print(CHUTES_LADDERS)
        print(new_energy)
    if random.uniform(0.0, 1.0) < math.exp(- beta * (new_energy - energy)):
        n_accept += 1
        energy = new_energy
        if energy < best_energy:
           best_energy = energy
           best_CHUTES_LADDERS = CHUTES_LADDERS.copy()
    else:
        CHUTES_LADDERS = old_CHUTES_LADDERS.copy()
        
print(best_CHUTES_LADDERS)
print(best_energy)

#34.76163891429752
#{99: 13, 98: 75, 19: 38, 5: 41, 57: 37, 4: 0, 46: 57, 74: 34, 22: 58, 15: 50}
#5.138250415370749e-06

#Solution FOUND!!!
#{99: 9, 2: 66, 11: 44, 35: 33, 7: 37, 87: 40, 5: 21, 42: 43, 4: 9, 68: 55}
#9.16281294394139e-07
# Hmmm, that's only 5 digits accurate 66.97870409127412 ( vs 66.978705007555420778)
