#/bin/bash

./main 8 > ./output/eccentric_anomaly_bisection.txt
gnuplot -e "data='./output/eccentric_anomaly_bisection.txt'; out='./output/keplerian_orbit.svg'" ./gnuplot/keplerian_orbit.plt
