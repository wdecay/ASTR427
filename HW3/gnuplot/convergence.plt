set terminal svg size 600,800
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

set logscale y 2
plot data1 using (abs(v1-$1)) with linespoints linestyle 1 notitle,\
     data2 using (abs(v2-$1)) with linespoints linestyle 2 notitle

