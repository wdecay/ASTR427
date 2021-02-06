set terminal png size 600, 600
set output out
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 2
set style line 3 linecolor rgb '#11aa33' linetype 1 linewidth 2

set title "Convergence"
set xlabel "log_2(h)" enhanced
set ylabel "log_2|x_n(30)-x_e(30)|" enhanced
set grid
set size ratio -1
set size square

set key bottom
plot data using 1:2 with linespoints linestyle 1 title "Euler", \
     data using 1:3 with linespoints linestyle 2 title "Leapfrog", \
     data using 1:4 with linespoints linestyle 3 title "Runge-Kutta"
