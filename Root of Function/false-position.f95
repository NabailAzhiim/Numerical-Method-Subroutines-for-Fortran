!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
!!!   Author : M. 'Anin Nabail 'Azhiim   !!!
!!!   Last modified : 07/09/2024 22.47   !!!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

!! define function f(x) then call the subroutine falseposition(xa,xb,tol,xc,Nit) !!


!! function f(x) !!
FUNCTION f(x) RESULT(z)

 IMPLICIT NONE

 REAL,INTENT(IN):: x
 REAL::z

 z = cos(x) - x ! input your function here, e.g. f(x) = cos(x) - x
 
 RETURN

END FUNCTION f


!! subroutine to find the root of f(x) using false position method !!
SUBROUTINE falseposition(xa,xb,tol,xc,Nit)

 IMPLICIT NONE

 REAL,INTENT(IN):: xa,xb ! initial guess (left,right)
 REAL,INTENT(IN):: tol ! tolerance
 REAL,INTENT(OUT):: xc ! result
 INTEGER,INTENT(OUT):: Nit ! number of iterations
 
 REAL:: a,b,c,c0,k
 INTEGER:: it
 
 ! check initial guess
 IF (f(xa)*f(xb) > 0.) THEN
   WRITE(*,*) "Error: The function must have opposite signs at xa and xb."
   xc = 0.
   Nit = 0
   RETURN
 END IF
 
 c0 = (xa*f(xb)-xb*f(xa))/(f(xb)-f(xa))
 it = 0

 ! check which initial guess is held constant
 IF (f(xa)*f(c0) < 0.) THEN
  a = xa
  b = xb
 END IF
 IF (f(xb)*f(c0) < 0.) THEN
  a = xb
  b = xa
 END IF

 ! false position method
 DO
  b = c0
  c = (a*f(b)-b*f(a))/(f(b)-f(a))
  k = abs((c-c0)/c)
  it = it+1
  IF (k < tol) EXIT
  c0 = c
 END DO

 xc = c
 Nit = it
 
 RETURN

END SUBROUTINE falseposition
