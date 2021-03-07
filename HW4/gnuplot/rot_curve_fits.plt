set terminal pngcairo size 700,500
set output out

v(vinf, r0, x) = vinf*(1 - exp(-x/r0))

set key right bottom
set title 'Rotation curve fitting'
set xlabel 'r, kpc'
set ylabel 'v, km/s'

vinf0 = 100.0
r00 = 3.0
# parsing
c(l, c) = sprintf("awk 'NR==%i {print $%i}' %s", l, c, data)
m1 = system(c(3, 1))
m2 = system(c(4, 1))
vinf1 = real(system(c(3, 2)))
vinf2 = real(system(c(4, 2)))
r01 = real(system(c(3, 3)))
r02 = real(system(c(4, 3)))

ttl(m, vinf, r0) = sprintf("%s (v_{inf} = %.3f, r_0 = %.3f)", m, vinf, r0)

plot 'rot.csv' with points pt 7 title 'Data',\
     v(vinf1, r01, x) with lines dt 5 lw 2 lc 7 title ttl(m1, vinf1, r01),\
     v(vinf2, r02, x) with lines lw 2 title ttl(m2, vinf2, r02),\
     v(vinf0, r00, x) with lines dt 2 lw 2 lc 0 title ttl('Original model', vinf0 , r00)
