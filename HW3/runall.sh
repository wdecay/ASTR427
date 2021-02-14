#/bin/bash

./main 8 > ./output/eccentric_anomaly_bisection.txt
gnuplot -e "data='./output/eccentric_anomaly_bisection.txt'; out='./output/eccentric_anomaly_bisection.png'" ./gnuplot/eccentric_anomaly.plt
