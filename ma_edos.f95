!18/05/2020
! ma_edos.f95
! Elser Lopez (eladolfos@gmail.com)
!----------------------------------------
!    Programa SOLUCIONES PENDULO SIMPLE: Este programa encuentra la solucion para theta, omega
!    a la ecuacion diferencial de segundo orden del punedulo simple, utilizando los metodos:
!    Euler, Euler Modificado, Heun y Runge-Kutta de cuarto orden.
!    NOTA: si se cambia la precision de este programa, tambien se debe hacer en los modulos.   
!----------------------------------------
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.5.0
!    Requerimientos adicionales: requiere de los modulos: funcs, meapp_edo y p_types
!    Instrucciones de compilacion:
!    gfortran -Wall -pedantic -std=f95 -c -o p_types.o p_types.f95
!    gfortran -Wall -pedantic -std=f95 -c -o meapp_edo.o meapp_edo.f95
!    gfortran -Wall -pedantic -std=f95 -c -o funcs.o funcs.f95
!    gfortran -Wall -pedantic -std=f95 -o ma_edos.exe funcs.o p_types.o meapp_edo.o ma_edos.f95
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
!   1. INICIO
    program pendulo_simple
        use funcs
        use meapp_edo
        use p_types
        implicit none
!   2. Elegir: la precision con la que se desea trabajar.
        integer, parameter :: pr=sp ! seleccionar la precision, sp: precision simple, dp: doble, qp: cuadruple
!   3. Definir: variables auxiliares y del problema
        real(pr), ALLOCATABLE :: eu(:,:), euM(:,:), heun(:,:), rk42(:,:)
        integer :: N, i
        character(len=10) :: metodo !nombre del metodo
        real(pr), parameter :: PIR=3.14159265358979323846264338327950288 !valor 'Real'' de PI
        real(pr) :: h, x0, y0, pto, u0, ag
!   4. Leer: condiciones iniciales y variables de configuracion
        OPEN(unit=1, file='conditions.ini', status='old')
        READ(1,*) metodo !nombre del metodo
        READ(1,*) h !tamano del paso
        READ(1,*) pto !punto que se quiere aproximar
        READ(1,*) x0 !punto inicial
        READ(1,*) ag !angulo inicial
        READ(1,*) u0 ! u(x0) !volicidad angular inicial
        y0=ag*(PIR/180) ! y(x0), convertido a radianes
        CLOSE(1)

        N=Int((pto-x0)/h)+1 ! por la condiciones iniciales
!   5. Reservar: memoria dinamica
        ALLOCATE(eu(N,3))
        ALLOCATE(euM(N,3))
        ALLOCATE(heun(N,3))
        ALLOCATE(rk42(N,3))
        eu=0
        euM=0
        heun=0
        rk42=0
!   6. Si: se requiere el metodo de Euler entonces:
        IF(metodo=='euler') THEN
            eu=EULER2do(h,pto,x0,y0,u0,Spen)
            !imprimir la tabla de con los valores x, y, u
            DO i=1, N
                WRITE(*,*) eu(i,1), eu(i,2), eu(i,3)
            END DO
        END IF
!   7. Si: se requiere el metodo de Euler modificado entonces:
        IF(metodo=='eulerMod') THEN
            euM=EULER2doMod(h,pto,x0,y0,u0,Spen)
            !imprimir la tabla de con los valores x, y, u
            DO i=1, N
                WRITE(*,*) euM(i,1), euM(i,2), euM(i,3)
            END DO
        END IF
!   8. Si: se requiere el metodo de Heun entonces:
        IF(metodo=='heun') THEN
            heun=HEUN2do(h,pto,x0,y0,u0,Spen)
            !imprimir la tabla de con los valores x, y, u
            DO i=1, N
                WRITE(*,*) heun(i,1), heun(i,2), heun(i,3)
            END DO
        END IF
!   9. Si: se requiere el metodo de Runge-Kutta de cuarto orden entonces:
        IF(metodo=='rk4') THEN
            rk42=RK42do(h,pto,x0,y0,u0,Spen)
            !imprimir la tabla de con los valores x, y, u
            DO i=1, N
                WRITE(*,*) rk42(i,1), rk42(i,2), rk42(i,3)
            END DO
        END IF        
!   10. Liberar: memoria dinamica
        DEALLOCATE(eu)
        DEALLOCATE(euM)
        DEALLOCATE(heun)
        DEALLOCATE(rk42)      
!   11. FIN
    end program pendulo_simple


