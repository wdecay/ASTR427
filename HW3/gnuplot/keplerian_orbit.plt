set terminal svg size 900, 450
set output out
set title "Elliptical orbit"

set style line 1 linecolor rgb '#0060ad' linetype 7 linewidth 2
set style line 2 linecolor rgb '#555555' linetype 1 linewidth 1

set xlabel "a (cos(E)-e)"
set ylabel "b sin(E)"
set xrange [-2.0:0.2]
set yrange [-0.5:0.5]

set grid
set size ratio -1
# set size square

e = 0.9
a = 1
b = sqrt(1-e**2)

set multiplot
set parametric
plot [0:2*pi] a*(cos(t)-e), b*sin(t) linestyle 1 notitle
unset parametric
plot data using (0):(0):(a*(cos($2)-e)):(b*sin($2)) with vectors linestyle 2 notitle

set object circle at 0,0 radius char 1 \
    fillcolor rgb 'red' fillstyle solid noborder
