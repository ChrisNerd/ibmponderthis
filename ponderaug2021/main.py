
import numpy as np
import scipy.optimize
import scipy.stats as stats
import matplotlib.pyplot as plt
import itertools
from scipy.sparse import dok_matrix
from scipy.sparse.linalg import spsolve
from scipy.sparse import identity


# Press Shift+F10 to execute it or replace it with your code.
# Press Double Shift to search everywhere for classes, files, tool windows, actions, and settings.
def print_hi(name):
    # Use a breakpoint in the code line below to debug your script.
    print(f'Hi, {name}')  # Press Ctrl+8 to toggle the breakpoint.


# Press the green button in the gutter to run the script.
if __name__ == '__main__':
    print_hi('PyCharm')

# See PyCharm help at https://www.jetbrains.com/help/pycharm/

total_number_of_players = 5

def calculate_tau_from_probabilities(probabilities, make_plots=False):
    global number_of_players_in_current_round, base_list_of_players, game, shooter, b, list_of_games_of_size_n_or_less
    list_of_games = []
    list_of_sizes = []
    # for numPlayersLeft in range(maxPlayers, 0, -1):
    i = 0
    for number_of_players_in_current_round in range(total_number_of_players + 1):
        for base_list_of_players in itertools.combinations(range(total_number_of_players),
                                                           number_of_players_in_current_round):
            for current_game in list(
                    map(lambda rotate_amount: list(base_list_of_players[rotate_amount:]) +
                                              list(base_list_of_players[:rotate_amount]),
                        range(number_of_players_in_current_round))):
                list_of_games.append(current_game)
                i = i + 1
        list_of_sizes.append(i)
    # Q is going to be a list of matrices, where Q[0] will be the 30x30 entries corresponding to the length-2 states.
    # R[0] will be the 30x6 matrix that's easy to populate right at the start.
    # R[1] will prepend a bunch of zeros above it, as that would represent the transitions from length-3 states
    # to absorbing states
    # This will be the size of R, since only and all games of 2 can have a winner (lead to an absorbing state).
    # number_of_games_of_size_2 = scipy.special.comb(total_number_of_players, 2, exact=True) * 2
    number_of_games_of_size_2 = list_of_sizes[2] - list_of_sizes[1]
    list_of_games_of_size_2 = list(filter(lambda l: len(l) == 2, list_of_games))
    r = dok_matrix((number_of_games_of_size_2, total_number_of_players), dtype=np.float64)
    qZero = dok_matrix((number_of_games_of_size_2, number_of_games_of_size_2), dtype=np.float64)
    for i, game in enumerate(list_of_games_of_size_2):
        shooter = game[0]
        r[i, shooter] = probabilities[shooter]
        j = list_of_games_of_size_2.index([game[1], game[0]])
        # will probably need to modify i with list_of_sizes on the next line
        qZero[i, j] = 1.0 - probabilities[shooter]
    b = spsolve(identity(number_of_games_of_size_2) - qZero, r)
    if make_plots:
        fig, (ax1, ax2, ax3) = plt.subplots(1, 3)
        fig.suptitle('Horizontally stacked subplots')
        ax1.set_title("Q0")
        ax1.imshow(qZero.todense(), cmap='hot')
        ax1.set_xticks(np.arange(len(list_of_games_of_size_2)))
        plt.xticks(rotation='vertical')
        ax1.set_xticklabels(list_of_games_of_size_2)
        ax1.set_yticks(np.arange(len(list_of_games_of_size_2)))
        ax1.set_yticklabels(list_of_games_of_size_2)
        ax2.set_title("R0")
        ax2.set_xticks(np.arange(total_number_of_players))
        ax2.set_yticks(np.arange(len(list_of_games_of_size_2)))
        ax2.set_yticklabels(list_of_games_of_size_2)
        ax2.imshow(r.todense(), cmap='hot')
        ax3.set_title("B")
        ax3.imshow(b.todense(), cmap='hot')
        ax3.set_xticks(np.arange(total_number_of_players))
        ax3.set_yticks(np.arange(len(list_of_games_of_size_2)))
        ax3.set_yticklabels(list_of_games_of_size_2)
        plt.show()  # block=False)
    # def create_new_matrices(numPlayers):
    for number_of_players_in_current_round in range(3, total_number_of_players + 1):
        list_of_games_of_size_n = list(filter(lambda l: len(l) == number_of_players_in_current_round, list_of_games))
        list_of_games_of_size_n_or_less = list(
            filter(lambda l: len(l) <= number_of_players_in_current_round, list_of_games))

        r.resize((list_of_sizes[number_of_players_in_current_round] - total_number_of_players, total_number_of_players))
        #    plt.imshow(r, cmap='hot')
        #    plt.show()

        qZero.resize((list_of_sizes[number_of_players_in_current_round] - total_number_of_players,
                      list_of_sizes[number_of_players_in_current_round] - total_number_of_players))
        for i, game in enumerate(list_of_games_of_size_n):
            shooter = game[0]
            # here we're going to have to find the max
            # we shoot them, then rotate
            # [x for x in m if x != 'a']
            candidates = list(map(lambda toShoot: [x for x in game if x != toShoot][1:] + [shooter], game[1:]))
            # bug here... going for is the max candidate, not the index of the max candidate
            going_for = max(candidates,
                            key=lambda c: b[
                                list_of_games_of_size_n_or_less.index(c) - total_number_of_players, shooter])
            # bug on this line

            qZero[i + list_of_sizes[number_of_players_in_current_round - 1] - total_number_of_players,
                  list_of_games_of_size_n_or_less.index(going_for) - total_number_of_players] = probabilities[shooter]

            j = list_of_games_of_size_n.index(game[1:] + [game[0]])
            qZero[i + list_of_sizes[number_of_players_in_current_round - 1] - total_number_of_players,
                  j + list_of_sizes[number_of_players_in_current_round - 1] - total_number_of_players] = 1.0 - \
                                                                                                         probabilities[
                                                                                                             shooter]

        #    plt.title('Q')
        #    plt.imshow(qZero, cmap='hot')
        #    plt.show()

        #    N = []
        #    nZero = np.linalg.inv(np.identity(len(rotatedPlayersListLengths)) - qZero)
        #    N.append(nZero)
        #    plt.title('N')
        ##    plt.imshow(nZero, cmap='hot')
        #   plt.show()

        # Awesome! Now we have q0 and n0 which are the 30x30 matrices.
        #    B = []
        # The @ operator can be used as a shorthand for np.matmul on ndarrays.
        # bZero = nZero @ r_zero
        # plt.imshow(bZero, cmap='hot')
        # plt.title('B')
        # plt.show()
        b.resize(r.shape)
        b = spsolve(identity(list_of_sizes[number_of_players_in_current_round] - total_number_of_players) - qZero, r)
        if make_plots:
            fig, (ax1, ax2, ax3) = plt.subplots(1, 3)
            fig.suptitle('Horizontally stacked subplots')
            ax1.set_title("Q0")
            ax1.imshow(qZero.todense(), cmap='hot')
            ax1.set_xticks(np.arange(len(list_of_games_of_size_n_or_less) - total_number_of_players))
            plt.xticks(rotation='vertical')
            ax1.set_xticklabels(list_of_games_of_size_n_or_less[total_number_of_players:], rotation='vertical')
            ax1.set_yticks(np.arange(len(list_of_games_of_size_n_or_less) - total_number_of_players))
            ax1.set_yticklabels(list_of_games_of_size_n_or_less[total_number_of_players:])
            ax2.set_title("R0")
            ax2.set_xticks(np.arange(total_number_of_players))
            ax2.set_yticks(np.arange(len(list_of_games_of_size_n_or_less) - total_number_of_players))
            ax2.set_yticklabels(list_of_games_of_size_n_or_less[total_number_of_players:])
            ax2.imshow(r.todense(), cmap='hot')
            ax3.set_title("B")
            ax3.imshow(b.todense(), cmap='hot')
            ax3.set_xticks(np.arange(total_number_of_players))
            ax3.set_yticks(np.arange(len(list_of_games_of_size_n_or_less) - total_number_of_players))
            ax3.set_yticklabels(list_of_games_of_size_n_or_less[total_number_of_players:])
            plt.show()  # block=False)
    # transistion matrix, or right stochastic matrix
    # P = [
    # [I R]
    # [0 Q]]
    # fundamental matrix
    # N = Sum from k=0 to infinity Q^k
    # N = (I-Q)^-1
    # Absorbing probabilities
    # B = NR
    # Alternatively
    # B(i,j) = lim k->infinity P^k (i,j)
    # Since we're solving B = (I-Q)^-1 R
    # We can rearrange this to (I-Q) B = R
    # which looks a lot like A X = B, the general lineal algebra equation.
    # Scipy has a solver for this that doesn't use inverses, and can take advantage of sparse matrices.
    # Kendall’s tau is a measure of the correspondence between two rankings. Values close to 1 indicate strong agreement,
    # values close to -1 indicate strong disagreement.
    print("Final probabilities of winning")
    index_of_original_game = list_of_games.index(list(range(total_number_of_players))) - total_number_of_players
    print(index_of_original_game)
    print(b.getrow(index_of_original_game).toarray().ravel())
    print(probabilities)
    print(np.argsort(b.getrow(index_of_original_game).toarray().ravel()))
    print(np.argsort(probabilities))

    tau, p_value = stats.kendalltau(b.getrow(index_of_original_game).toarray().ravel(), probabilities)
    if make_plots:
        plt.plot(b.getrow(index_of_original_game).toarray().ravel(), label='b plot')
        plt.plot(probabilities, label='prob plot')
        plt.legend()
        plt.show()
    #print("Tau p")
    print(tau)
    #print(p_value)
    return 1 - tau

#probabilities_that_solves_the_6_case = [0.80124574, 0.79016234, 0.73239589, 0.79263102, 0.5752745 , 0.08999134]
#out6 = calculate_tau_from_probabilities(probabilities_that_solves_the_6_case, make_plots=True)
print("end")
#print(out6)
solution_to_case_6_input_probabilities = [0.80124574, 0.79016234, 0.73239589, 0.79263102, 0.5752745, 0.08999134]
solution_to_case_6_output_probabilities =[0.39850343, 0.14684528, 0.11251467, 0.16105667, 0.0960006, 0.08507934]
#[5 4 2 1 3 0]
#[5 4 2 1 3 0]

# For the case of 8 players
#fun: 2.220446049250313e-16
#message: ['Maximum number of iteration reached']
#nfev: 16064
#nhev: 0
#nit: 1000
#njev: 7
#status: 0
#success: True
solution_to_case_8_input_probabilities =[0.4812741, 0.35734385, 0.03418574, 0.90561896, 0.38528354,
          0.10312144, 0.78590653, 0.26672937]
solution_to_case_8_output_probabilities = [0.18519959, 0.08851481, 0.04121023, 0.20478868, 0.17460725, 0.04465235,
 0.18553397, 0.07549313]
# calculate_tau_from_probabilities(solution_to_case_8_input_probabilities, True)
#[2 5 7 1 4 0 6 3]
#[2 5 7 1 4 0 6 3]
#calculate_tau_from_probabilities([.25, .5, 1], True)
# should get .29375, 425, .28125
# 1 2 3
# 2, 3 1

#[0.29375 0.425   0.28125]
#[0.25, 0.5, 1]
#[2 0 1]
#[0 1 2]

calculate_tau_from_probabilities([.5, .2, 0.05, .85, .1], True)
# should get .205 .196 .178 .238 .183
# 4 3 1 5 2
#[0.20493899 0.19585636 0.17782349 0.23800758 0.18337359]
#[0.5, 0.2, 0.05, 0.85, 0.1]
#[2 4 1 0 3]
#[2 4 1 0 3]

#optimizer_out = scipy.optimize.dual_annealing(calculate_tau_from_probabilities,
#                                            bounds=list(zip([0]*total_number_of_players, [1]*total_number_of_players)))
#print(optimizer_out)
