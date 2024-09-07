!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   Author : M. 'Anin Nabail 'Azhiim   !!!
!!!   Last modified : 07/09/2024 23.01   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! define function f(x) then call the subroutine secant(xa,xb,tol,xc,Nit) !!

!! function f(x) !!
FUNCTION f(x) RESULT(z)

 IMPLICIT NONE

 REAL,INTENT(IN):: x
 REAL:: z

 z = cos(x) - x ! input your function here, e.g. f(x) = cos(x) - x
 
 RETURN

END FUNCTION f


!! subroutine to find the root of f(x) using secant method !!
SUBROUTINE secant(xa,xb,tol,xc,Nit)

 IMPLICIT NONE
 
 REAL,INTENT(IN):: xa,xb ! initial guess (left,right)
 REAL,INTENT(IN):: tol ! tolerance
 REAL,INTENT(OUT):: xc ! result
 INTEGER,INTENT(OUT):: Nit ! number of iterations

 REAL:: a,b,c,c0,k
 INTEGER:: it 
 
 a = xa
 b = xb
 c0 = (a*f(b)-b*f(a))/(f(b)-f(a))
 it = 0

 ! secant method
 DO
  c = (b*f(c0)-c0*f(b))/(f(c0)-f(b)) 
  k = abs((c-c0)/c)
  it = it+1
  IF (k < tol) EXIT
  b = c0
  c0 = c
 END DO

 xc = c
 Nit = it

 RETURN

END SUBROUTINE secant
