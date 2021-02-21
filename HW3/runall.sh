#/bin/bash

make

./main 1 2.0 > ./output/sr_bisection.txt
./main 2 2.0 > ./output/sr_newton.txt

./main 3 0.5 > ./output/kepler_0.5_bisection.txt
./main 4 0.5 > ./output/kepler_0.5_newton.txt

./main 3 0.9 > ./output/kepler_0.9_bisection.txt
./main 4 0.9 > ./output/kepler_0.9_newton.txt

./main 6 0.9 > ./output/eccentric_anomaly_bisection.txt
./main 6 0.96714 > ./output/eccentric_anomaly_halley.txt

gnuplot -e "data1='./output/sr_bisection.txt'; data2='./output/sr_newton.txt';\
out='./output/sr_conv.png';subtitle='square root of 2.0'" ./gnuplot/convergence.plt

gnuplot -e "data1='./output/kepler_0.5_bisection.txt'; data2='./output/kepler_0.5_newton.txt';\
out='./output/kepler_0.5_conv.png';subtitle='Kepler''s equation, e=0.5'" ./gnuplot/convergence.plt

gnuplot -e "data1='./output/kepler_0.9_bisection.txt'; data2='./output/kepler_0.9_newton.txt';\
out='./output/kepler_0.9_conv.png';subtitle='Kepler''s equation, e=0.9'" ./gnuplot/convergence.plt

gnuplot -e "e=0.9; data='./output/eccentric_anomaly_bisection.txt';\
out='./output/eccentric_anomaly.png'" ./gnuplot/eccentric_anomaly.plt

gnuplot -e "e=0.9; data='./output/eccentric_anomaly_bisection.txt';\
out='./output/keplerian_orbit.png'" ./gnuplot/keplerian_orbit.plt

gnuplot -e "e=0.96714; data='./output/eccentric_anomaly_halley.txt';\
out='./output/keplerian_orbit_halley.png'" ./gnuplot/keplerian_orbit.plt
