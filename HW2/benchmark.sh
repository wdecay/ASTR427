#!/bin/bash

for h in 1.0 0.3 0.1 0.03 0.01
do
    OUTPUT=./output/step${h}
    ./problem1 $h > "${OUTPUT}.txt"
    gnuplot -e "data='${OUTPUT}.txt'; out='${OUTPUT}.png'; h='${h}'" solutions.plt
    rm "${OUTPUT}.txt"
done
