set terminal pngcairo size 400/sqrt(1-e**2), 450
set output out
set title "Elliptical orbit"

set style line 1 dt 3 linewidth 2 lc rgb '#0060ad' 
set style line 2 linecolor rgb '#555555' linetype 1 linewidth 1

a = 1
b = sqrt(1-e**2)

set xlabel "a (cos(E)-e)"
set ylabel "b sin(E)"
unset grid
set size ratio -1
set xrange [-a * (1 + e) - a*e*0.05:a*(1 - e) + a*e*0.05]
set yrange [-1.1*b:1.1*b]

set multiplot
set parametric
plot [0:2*pi] a*(cos(t)-e), b*sin(t) linestyle 1 notitle
unset parametric

set object circle at 0,0 front radius char 1 \
    fillcolor rgb 'orange' fillstyle solid noborder

plot data using (0):(0):(a*(cos($2)-e)):(b*sin($2)) with vectors linestyle 2 notitle

