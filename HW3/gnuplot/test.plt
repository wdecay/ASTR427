set terminal png size 600,800
set output out
set title "test"

set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1

set grid

f(x) = 2*pi - x + 0.9 * sin(x)

set xrange [0:2*pi]
plot f(x) title 'f(E3)' with lines linestyle 2
