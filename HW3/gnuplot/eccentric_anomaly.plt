set terminal png size 600,600
set output out
set title "Eccentric anomaly"

set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1

set xlabel "cos(E)"
set ylabel "sin(E)"
set xrange [-1.1:1.1]
set yrange [-1.1:1.1]

set grid
set size ratio -1
set size square

plot data using (cos($2)):(sin($2)) with linespoints linestyle 1 notitle
