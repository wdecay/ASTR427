set terminal png size 600,800
set output out
set title "test"
set multiplot layout 3,1
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1

set grid
; set key box opaque
set key outside horizontal below

; set xlabel "t"
unset xlabel
; set ylabel "x"
unset ylabel
f(x) = cos(x)

set title sprintf("Euler; h=%s", h)
plot data using 1:2 smooth csplines linestyle 1 title 'Numerical',  \
     f(x) title 'cos(t)' with lines linestyle 2

set title sprintf("Leapfrog; h=%s", h)
plot data using 1:4 smooth csplines linestyle 1 title 'Numerical',  \
     f(x) title 'cos(t)' with lines linestyle 2

set title sprintf("Runge-Kutta; h=%s", h)
plot data using 1:6 smooth csplines linestyle 1 title 'Numerical',  \
     f(x) title 'cos(t)' with lines linestyle 2
