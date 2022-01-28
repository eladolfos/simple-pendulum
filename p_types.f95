! 20/03/2020
! p_types.f95
! Elser Lopez (eladolfos@gmail.com)
!----------------------------------------
!    MODULO: definicion sencilla de precision
!----------------------------------------

!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.5.0
!    Instrucciones de compilación: no requiere nada
!    gfortran -Wall -pedantic -std=f95 -c -o p_types.o p_types.f95
!    
!    Copyright (C) 2020
!    E.A. López
!    eladolfos@gmail.com
!
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

MODULE p_types
     IMPLICIT NONE
          !integer bits
          integer, parameter :: I1B=selected_int_kind(2) !one bit
          integer, parameter :: I2B=selected_int_kind(4) !two bits
          integer, parameter :: I4B=selected_int_kind(9) !four bits
          integer, parameter :: I8B=selected_int_kind(18) !eight bits
          integer, parameter :: I16B=selected_int_kind(38) ! sixteen bits
          
          !real precision
          integer, parameter ::  sp = kind(1.0) !single precision
          integer, parameter ::  dp = selected_real_kind(2*precision(1.0_sp)) !double presicion
          integer, parameter ::  qp = selected_real_kind(2*precision(1.0_dp)) !quad preciesion

END MODULE p_types
