function hout = h(S)
##subject to
#h(x) >= 0
#S(1)*S(7) <=2
#S(1)*S(7)-2 <=0
#-S(1)*S(7)+2 >=0
hout = -S(1)*S(7)+2
endfunction