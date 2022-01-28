!18/05/2020
! meapp_edo.f95
! Elser Lopez (eladolfos@gmail.com)
!----------------------------------------
!   MODULO METODOS DE APROXIMACION EDOS:
!   Contiene los metodos de aproximacion utilizados para resolver sistemas de ecuaciones diferenciales
!   de primer orden, los cuales son: Euler, Euler Modificado, Heun y Runge-Kutta de Cuarto Orden.
!   NOTA: si se cambia la precision aqui, tambien se debe cambiar en los demas modulos
!   y programas que utilizan este modulo  
!----------------------------------------
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.5.0
!    Requerimientos adicionales: requiere del modulo p_types
!    Instrucciones de compilacion:
!    gfortran -Wall -pedantic -std=f95 -c -o p_types.o p_types.f95
!    gfortran -Wall -pedantic -std=f95 -c -o meapp_edo.o meapp_edo.f95
!
!    Copyright (C) 2020
!    E.A. López
!    eladolfos@gmail.com
!
!----------------------------------------
!    This program is free software: you can redistribute it and/or
!    modify it under the terms of the GNU General Public License as
!    published by the Free Software Foundation, either version 3 of
!    the License, or (at your option) any later version.
!
!    This program is distributed in the hope that it will be useful,
!    but WITHOUT ANY WARRANTY; without even the implied warranty of
!    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!    General Public License for more details.
!
!    You should have received a copy of the GNU General Public License
!    along with this program.  If not, see
!    <http://www.gnu.org/licenses/>.
!
!

module meapp_edo
    use p_types
    implicit none
!---------------------------------IMPORTANTE------------------------------
    integer, parameter :: tp=sp ! seleccionar la precision, sp: precision simple, dp: doble, qp: cuadruple
!-------------------------------------------------------------------------
    contains
!   1. INICIO
    function EULER2do(h,pto,x0,y0,u0,f)
    !metod de Euler para la solcion de EDO de segundo orden
        implicit none
        !variabls del metodo
!   2. Definir: variables auxiliares y del problema
        integer :: N, i !numero de puntos que se guardaran
        real(tp) :: h, pto, x0, y0, u0
        real(tp) :: xn, yn, un
        real(tp), dimension(INT((pto-x0)/h)+1,3) :: EULER2do !matriz de tres columnas la primera guarda los x_k, en la segunda los y_k, y en la tercer los u_k
        real(tp) :: f !funcion que se tomara del modulo funcs
        EULER2do=0
        N=INT((pto-x0)/h)
!   3. Asignar: las conddiciones iniciales a la matriz
        !guardo la condiciones iniciales en
        EULER2do(1,1)=x0
        EULER2do(1,2)=y0
        EULER2do(1,3)=u0
!   4. Hacer desde i=1 hasta N
        DO i=1, N
            xn=x0+h
            yn=y0+h*u0
            un=u0+h*f(x0,y0,u0)
            !actualizo los valors de xn, yn, un
            x0=xn
            y0=yn
            u0=un
            !los agrego a la matriz
            EULER2do(i+1,1)=x0
            EULER2do(i+1,2)=y0
            EULER2do(i+1,3)=u0
        END DO
!   5. Retornar: matriz de (N+1)x3
    return
!   6. FIN
    end function EULER2do

!   1. INICIO
    function EULER2doMod(h,pto,x0,y0,u0,f)
    !metodo de Euler Mejorado para la solucion de una edo de segundo orden
        implicit none
!   2. Definir: variables auxiliares y del problema
        integer :: i, N
        real(tp) :: h, pto, x0, y0, u0
        real(tp), dimension(INT((pto-x0)/h)+1,3) :: EULER2doMod !matriz de tres columnas la primera guarda los x_k, en la segunda los y_k, y en la tercer los u_k
        real(tp) :: xn, yn, un, zn, wn
        real(tp) :: f !funcion que se tomara del modulo funcs
        EULER2doMod=0
        N=INT((pto-x0)/h)
        !guardo la condiciones iniciales en la matriz
        EULER2doMod(1,1)=x0
        EULER2doMod(1,2)=y0
        EULER2doMod(1,3)=u0
        DO i=1, N
            !WRITE(*,*) x0, y0, u0
            xn=x0+h
            wn=u0+h*u0
            yn=y0+0.5*h*(u0+wn)
            zn=u0+h*f(x0,y0,u0)
            un=u0+0.5*h*(f(x0,y0,u0)+f(xn,yn,zn))
            !actualiza los valores de x0,y0, u0
            x0=xn
            y0=yn
            u0=un
            !los agrego a la matriz
            EULER2doMod(i+1,1)=x0
            EULER2doMod(i+1,2)=y0
            EULER2doMod(i+1,3)=u0
        END DO
    return
    end function EULER2doMod

    function RK42do(h,pto,x0,y0,u0,f)
    !funcion que calcula la solucion para una edo de segundo orden usando RK4
    !por algun motivo solo funciona con esta definicion:
    !http://www.sc.ehu.es/sbweb/fisica_/numerico/diferencial/segundo.html
        implicit none
        real(tp) :: h, x0, u0, y0, pto
        integer :: i, N
        real(tp), dimension(INT((pto-x0)/h)+1,3) :: RK42do !matriz de tres columnas la primera guarda los x_k, en la segunda los y_k, y en la tercer los u_k
        real(tp) :: xn=0, yn=0, un=0
        real(tp) :: k1=0, k2=0, k3=0, k4=0
        real(tp) :: m1=0, m2=0, m3=0, m4=0
        real(tp) :: f
        RK42do=0
        N=INT((pto-x0)/h)
        !guardo la condiciones iniciales en la matriz
        RK42do(1,1)=x0
        RK42do(1,2)=y0
        RK42do(1,3)=u0
        DO i=1, N !hace el loop hasta que se llega al punto
            xn=x0+h
            m1=h*u0
            k1=h*f(x0,y0,u0)
            m2=h*(u0+0.5*k1)
            k2=h*f(x0+0.5*h,y0+0.5*m1,u0+0.5*k1)
            m3=h*(u0+0.5*k2)
            k3=h*f(x0+0.5*h,y0+0.5*m2,u0+0.5*k2)
            m4=h*(u0+0.5*k3)
            k4=h*f(x0+h,y0+m3,u0+k3)
            un=u0+(1.0/6)*(k1+2*k2+2*k3+k4)
            yn=y0+(1.0/6)*(m1+2*m2+2*m3+m4)
            y0=yn !actualiza los valores de y0
            x0=xn !actualiza el valor de x0
            u0=un !actualiza el calor de u0
            !los agrego a la matriz
            RK42do(i+1,1)=x0
            RK42do(i+1,2)=y0
            RK42do(i+1,3)=u0
        END DO
    return
    end function RK42do

    function HEUN2do(h,pto,x0,y0,u0,f)
        implicit none
        real(tp) :: h, x0, u0, y0, pto
        real(tp), dimension(INT((pto-x0)/h)+1,3) :: HEUN2do !matriz de tres columnas la primera guarda los x_k, en la segunda los y_k, y en la tercer los u_k
        real(tp) :: xn, yn, un, z, w
        integer :: i, N
        real(tp) :: f
        HEUN2do=0
        N=INT((pto-x0)/h)
        !guardo la condiciones iniciales en la matriz
        HEUN2do(1,1)=x0
        HEUN2do(1,2)=y0
        HEUN2do(1,3)=u0
        DO i=1, N
            xn=x0+h
            w=u0+(1.0/3)*u0
            yn=y0+0.25*h*(u0+3*(u0+(2.0/3)*h*w))
            z=f(x0+h/3,y0+h/3,u0+(h/3)*f(x0,y0,u0))
            un=u0+0.25*h*(f(x0,y0,u0)+3*f(x0+(2.0/3)*h,y0+(2.0/3)*h,u0+(2.0/3)*h*z))
            y0=yn !actualiza los valores de y0
            x0=xn !actualiza el valor de x0
            u0=un !actualiza el calor de u0
            HEUN2do(i+1,1)=x0
            HEUN2do( i+1,2)=y0
            HEUN2do(i+1,3)=u0
        END DO
    return
    end function HEUN2do



end module meapp_edo