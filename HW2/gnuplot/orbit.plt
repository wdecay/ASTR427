set terminal png size 1000, 500
set output out
set multiplot layout 1,2 title sprintf("%s, h=%s", info, h)
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 1
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1

set grid
stats data using 6 nooutput

set xlabel "x"
set ylabel "y"
set xrange [-1.1:1.1]
set yrange [-1.1:1.1]
set size ratio -1
set size square
set key bottom
plot data using 2:3 with lines linestyle 1 notitle

unset xrange
unset yrange
set size nosquare
set xlabel "t"
set ylabel "E - <E>"
set key top
set label sprintf("<E> = %.8f  ", STATS_mean) at graph 1.0, 0.9 right front
plot data using 1:($6 - STATS_mean) smooth csplines linestyle 2 title "Energy fluctuation"
