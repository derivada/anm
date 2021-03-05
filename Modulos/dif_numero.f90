program dif_numeros

! Test de precision a la hora de operar con o sin el módulo
! compilacion: gfortran -o dif_numero.exe dif_numero.f90 
! ejecucion: .\dif_numero.exe

use mod_clreal
implicit none

print *, "2.-sqrt(2.)**2",                 2.-sqrt(2.)**2
print *, "2._clreal-sqrt(2.)**2",          2._clreal-sqrt(2.)**2
print *, "2.-sqrt(2._clreal)**2",          2.-sqrt(2._clreal)**2
print *, "2._clreal-sqrt(2._clreal)**2",   2.-sqrt(2._clreal)**2

end program dif_numeros