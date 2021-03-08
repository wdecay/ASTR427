#!/bin/bash

make

./main 1 > output/rot_curve_fits.txt
./main 2 > output/map.txt

cat output/rot_curve_fits.txt

gnuplot -e "data='./output/rot_curve_fits.txt';out='./output/rot_curve_fits.png'" \
        ./gnuplot/rot_curve_fits.plt

gnuplot -e "data='./output/map.txt';out='./output/map.png'" ./gnuplot/map.plt
