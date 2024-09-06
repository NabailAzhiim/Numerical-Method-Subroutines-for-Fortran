!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   Author : M. 'Anin Nabail 'Azhiim   !!!
!!!   Last modified : 06/09/2024 22.40   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! define function f(x) then call the subroutine bisection(xa,xb,tol,xc,Nit) !!

! subroutine to find the root of f(x) using bisection method
SUBROUTINE bisection(xa,xb,tol,xc,Nit)

 IMPLICIT NONE
 
 REAL,INTENT(IN):: xa,xb ! initial guess (left,right)
 REAL,INTENT(IN):: tol ! tolerance
 REAL,INTENT(OUT):: xc ! root of function
 INTEGER,INTENT(OUT):: Nit ! number of iterations

 REAL:: a,b,c,c0,k
 INTEGER::it 
 
 ! check initial guess
 IF (f(xa)*f(xb) > 0.) THEN
    WRITE(*,*) "Error: The function must have opposite signs at xa and xb."
    xc = 0.
    Nit = 0
    RETURN
 END IF

 a = xa
 b = xb
 c0 = 0.5*(a+b)
 it = 0

 DO
  IF (f(a)*f(c0) < 0.) b = c0
  IF (f(b)*f(c0) < 0.) a = c0
  c = 0.5*(a+b)
  k = abs((c-c0)/c)
  it = it+1
  IF (k < tol) EXIT
  c0 = c
 END DO
 
 xc = c
 Nit = it

 RETURN

END SUBROUTINE bisection


! function f(x)
FUNCTION f(x) RESULT(z)

 IMPLICIT NONE

 REAL,INTENT(IN):: x
 REAL::z

 z = cos(x) - x ! input your function here, e.g. f(x) = cos(x) - x
 
 RETURN

END FUNCTION f

