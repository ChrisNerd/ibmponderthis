S0=[1,2,3,5,8,6,3,9,10, 12 ,15,20];
#//| List(1, 5, 6, 7, 8, 13)
#//| List(2, 3, 4, 6, 7, 9)

S1=[1, 5, 6, 7, 8, 13, 2, 3, 4, 6, 7, 9]
[x, obj, info, iter, nf, lambda] = sqp (S1, @gcon, [], [],[],[],2000)
