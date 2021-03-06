set terminal pngcairo size 750,500
set output out
set view map

set palette defined (0 "blue", 0.5 "light-grey", 1 "light-red")
set cbrange [400:560]
set title "Least squares error"
set xlabel "v_0" enhanced
set ylabel "r_0" enhanced
splot data matrix using ($2+95):(($1+30)/10):3 with image

