# Simple Pendulum solutions
This program finds the solutions for angle theta and the angular velocity omega. With the methods: Euler, Euler modified, Heun and Runge–Kutta fourth-order.

Compilation procedure

    gfortran -Wall -pedantic -std=f95 -c -o p_types.o p_types.f95
    gfortran -Wall -pedantic -std=f95 -c -o meapp_edo.o meapp_edo.f95
    gfortran -Wall -pedantic -std=f95 -c -o funcs.o funcs.f95
    gfortran -Wall -pedantic -std=f95 -o ma_edos.exe funcs.o p_types.o meapp_edo.o ma_edos.f95


