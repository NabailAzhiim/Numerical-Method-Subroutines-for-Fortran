!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   Author : M. 'Anin Nabail 'Azhiim   !!!
!!!   Last modified : 07/09/2024 22.56   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! define function f(x) and its derivative f'(x) then call the subroutine newtonraphson(x0,tol,xc,Nit) !!



!! function f(x) !!
FUNCTION f(x) RESULT(z)

 IMPLICIT NONE

 REAL,INTENT(IN):: x
 REAL:: z

 z = cos(x) - x ! input your function here, e.g. f(x) = cos(x) - x
 
 RETURN

END FUNCTION f


!! derivative f'(x) !!
FUNCTION df(x) result(z)

 IMPLICIT NONE

 REAL,INTENT(IN):: x
 REAL:: z

 z = -sin(x) - 1 ! input your derivative of the function here, e.g. f'(x) = -sin(x) - 1
 
 RETURN

END FUNCTION df


!! subroutine to find the root of f(x) using Newton-Raphson method !!
SUBROUTINE newtonraphson(x0,tol,xc,Nit)

 IMPLICIT NONE
 
 REAL,INTENT(IN):: x0 ! initial guess
 REAL,INTENT(IN):: tol ! tolerance
 REAL,INTENT(OUT):: xc ! result
 INTEGER,INTENT(OUT):: Nit ! number of iterations

 REAL:: a,c,c0,k
 INTEGER:: it 
 
 a = x0
 c0 = a
 it = 0

 ! Newton-Raphson method
 DO
  c = c0 - f(c0)/df(c0)
  k = abs((c-c0)/c)
  it = it+1
  IF (k < tol) EXIT
  c0 = c
 END DO

 xc = c
 Nit = it
 
 RETURN

END SUBROUTINE newtonraphson
