set terminal pngcairo size 1500, 500
set output sprintf("output/dt_%.3f.png", dt)
set multiplot layout 1,3 title sprintf("Explicit forward differencing method with dt=%.3f", dt)
set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 1 
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1 dt 3

round(x) = x - floor(x) < 0.5 ? floor(x) : ceil(x)

set xlabel "t"
set ylabel "u(x, t)"
x = 0.4
dx = 0.1
set title sprintf("(a) x=%.3f", x)
r = round(x/dx) + 1
plot data using ($0*dt):5 with lines linestyle 1 notitle

r1 = round(0.12 / dt)
r2 = round(0.4 / dt)

set xlabel "x"
set ylabel "u(x,t)"

set title sprintf("(b) t=%.3f", r1 * dt)
plot data using ($1/10):3 matrix every :::r1::r1 with linespoints linestyle 2 notitle 

set title sprintf("(c) t=%.3f", r2 * dt)
plot data using ($1/10):3 matrix every :::r2::r2 with linespoints linestyle 2 title sprintf("t=%.3f", r2 * dt)
