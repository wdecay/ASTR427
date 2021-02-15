#/bin/bash

make

./main 8 0.9 > ./output/eccentric_anomaly_bisection.txt
gnuplot -e "e=0.9; data='./output/eccentric_anomaly_bisection.txt'; out='./output/keplerian_orbit.svg'" ./gnuplot/keplerian_orbit.plt
