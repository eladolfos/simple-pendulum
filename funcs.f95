!18/05/2020
! funcs.f95
! Elser Lopez (eladolfos@gmail.com)
!----------------------------------------
!   MODULO DE FUNCIONES:
!   Este modulo contiene la funcion utilizada para resolver el problema del pendulo simple
!   Cuando este se separa en un sistema de ecuaciones de primer orden.   
!----------------------------------------
!
!    Codificación del texto: UTF8
!    Compiladores probados: GNU Fortran (Xubuntu Linux) 7.5.0
!    Requerimientos adicionales: Requiere del moduy p_types
!    Instrucciones de compilacion:
!    gfortran -Wall -pedantic -std=f95 -c -o p_types.o p_types.f95
!    gfortran -Wall -pedantic -std=f95 -c -o funcs.o funcs.f95
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
module funcs
    use p_types
    implicit none
    integer, parameter :: tpf=sp ! seleccionar la precision, sp: precision simple, dp: doble, qp: cuadruple
  contains

    function SPen(x,y,u)
    !funcion para el pendulo simple con g/l=1
      implicit none
      real(tpf) :: SPen, k=0
      real(tpf), intent(in) :: x, y, u
      SPen=-sin(y)+k*(x+u)
    end function SPen

end module funcs