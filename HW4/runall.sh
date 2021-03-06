./main 3 > output/map.txt

gnuplot -e "data='./output/map.txt';out='./output/map.png'" ./gnuplot/map.plt
