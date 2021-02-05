set terminal png size 800, 400
set output out
set multiplot layout 1,2
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 1
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1

set grid
# set key box opaque
# set key outside horizontal below


# set title sprintf("Euler; h=%s", h)

set xlabel "x"
set ylabel "y"
set xrange [-1.1:1.1]
set yrange [-1.1:1.1]
set size ratio -1
set size square
set key bottom
plot data using 2:3 with lines linestyle 1 notitle

# set title sprintf("Leapfrog; h=%s", h)
unset xrange
unset yrange
set size nosquare
set xlabel "t"
set ylabel "E"
set key top
plot data using 1:6 smooth csplines linestyle 2 title "Energy"
