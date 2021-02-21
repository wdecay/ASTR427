set terminal pngcairo size 600,600
set output out
set title "Eccentric anomaly"

set style line 1 dt 3 linewidth 2 lc rgb '#0060ad' 
set style line 2 linecolor rgb '#222222' linetype 1 linewidth 1

set xlabel "cos(E)"
set ylabel "sin(E)"
set xrange [-1.1:1.1]
set yrange [-1.1:1.1]

set grid
set size ratio -1
set size square

set multiplot
set parametric
plot [0:2*pi] cos(t), sin(t) ls 1 notitle
unset parametric

plot data using (0):(0):(cos($2)):(sin($2)) with vectors linestyle 2 notitl