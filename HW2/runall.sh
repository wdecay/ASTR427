#!/bin/bash

run_steps () {
    for h in 1.00 0.30 0.10 0.03 0.01
    do
        OUTPUT=./output/$2_$3_step_${h}
        $1 $h > "${OUTPUT}.txt"
        gnuplot -e "data='${OUTPUT}.txt'; out='${OUTPUT}.png'; h='${h}'; info='$3'" ./gnuplot/$2.plt
        rm "${OUTPUT}.txt"
    done
    printf "\n$4\n"
}

run_convergence_test() {
    OUTPUT=./output/conv_test
    ./problem2 > "${OUTPUT}.txt"
    gnuplot -e "data='${OUTPUT}.txt'; out='${OUTPUT}.png'" ./gnuplot/$1.plt
    rm "${OUTPUT}.txt"
    printf "\n$2\n"
}

make
time run_steps ./problem2 cos "comparison" "[done] Problem 2 (a)" &
time run_convergence_test convergence "[done] Problem 2 (b)" &
time run_steps "./problem3 --rk" orbit "Runge-Kutta" "[done] Problem 3 (Runge-Kutta)" &
time run_steps "./problem3 --lf" orbit "Leapfrog" "[done] Problem 3 (Leapfrog)" &
wait
printf '\nGraphs saved to ./output\n'
