set terminal png size 700,500
set output out
set title "test"

set style line 1 linecolor rgb '#0060ad' linetype 1 linewidth 2
set style line 2 linecolor rgb '#dd181f' linetype 1 linewidth 1

set grid
stats data1 using 1 nooutput
stats data1 u (v1=$1) every ::STATS_records-1::STATS_records-1 nooutput

stats data2 using 1 nooutput
stats data2 u (v2=$1) every ::STATS_records-1::STATS_records-1 nooutput
# show variables all

set logscale y

minvalue = 2**(-60)
# set ytics 4*yzero,4
set ytics add ("0" minvalue)

# the clamping technique is borrowed from
# https://koutny.org/2020/10/04/logscale-gnuplot.html
clamp(x) = (x < minvalue) ? minvalue : x;

plot data1 using (clamp(abs(v1-$1))) with linespoints linestyle 1 notitle,\
     data2 using (clamp(abs(v2-$1))) with linespoints linestyle 2 notitle

