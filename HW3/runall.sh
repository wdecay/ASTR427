#/bin/bash

make

./main 6 0.9 > ./output/eccentric_anomaly_bisection.txt
gnuplot -e "e=0.9; data='./output/eccentric_anomaly_bisection.txt'; out='./output/keplerian_orbit.svg'" ./gnuplot/keplerian_orbit.plt

./main 1 2.0 > ./output/sr_bisection.txt
./main 2 2.0 > ./output/sr_newton.txt

./main 3 0.5 > ./output/kepler_0.5_bisection.txt
./main 4 0.5 > ./output/kepler_0.5_newton.txt

./main 3 0.9 > ./output/kepler_0.9_bisection.txt
./main 4 0.9 > ./output/kepler_0.9_newton.txt

gnuplot -e "data1='./output/sr_bisection.txt'; data2='./output/sr_newton.txt';out='./output/sr_conv.svg'" ./gnuplot/convergence.plt

gnuplot -e "data1='./output/kepler_0.5_bisection.txt'; data2='./output/kepler_0.5_newton.txt';out='./output/kepler_0.5_conv.svg'" ./gnuplot/convergence.plt

gnuplot -e "data1='./output/kepler_0.9_bisection.txt'; data2='./output/kepler_0.9_newton.txt';out='./output/kepler_0.9_conv.svg'" ./gnuplot/convergence.plt
