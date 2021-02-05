#!/bin/bash

run_steps () {
    for h in 1.00 0.30 0.10 0.03 0.01
    do
        OUTPUT=./output/$2_step_${h}
        $1 $h > "${OUTPUT}.txt"
        gnuplot -e "data='${OUTPUT}.txt'; out='${OUTPUT}.png'; h='${h}'" ./gnuplot/$2.plt
        rm "${OUTPUT}.txt"
        echo "${OUTPUT}.png saved"
    done
}

make
run_steps ./problem2 cos
run_steps ./problem3 orbit
