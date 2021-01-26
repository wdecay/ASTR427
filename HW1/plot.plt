set title "4th degree polynomial interpolation"
set terminal png size 800,600
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 2
set output 'interpolation.png'
set grid
set multiplot
set xlabel "x"
set ylabel "y"
m="tabulation.txt"
f(x) = 100 / x ** 2
plot m using 1:2 with linespoints linestyle 1 title 'Neville',  \
     f(x) title '100/x^2' with lines linestyle 2
