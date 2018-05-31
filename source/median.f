c median.f    []
c	parameter (n = 8)
c	dimension x(n)

c	x(1) = 1.90560
c	x(2) = 1.27506
c	x(3) = 2.35488
c	x(4) = 2.22930
c	x(5) = 1.90560
c	x(6) = 1.75282
c	x(7) = 2.01101
c	x(8) = 1.84767

c	write(6,'(''Data values:'')')
c	write(6,'(4e20.10)') x
c	call median(x,n,xmed,ierr)
c	print *, 'xmed = ', xmed
c	stop
c	end

	subroutine median(x,n,xmed,ierr)

c  use iterative method to determine the median:

c      N                                          __ N     xj
c     ___     xj - xmed                           >     -----------
c     \      ----------- = 0    ==>    xmed =     -- 1  |xj - xmed|
c     /      |xj - xmed|                         -----------------
c     ---                                         __ N      1
c     j=1                                         >     -----------
c                                                 -- 1  |xj - xmed|

c  modified by C. Stephens from routine mdian2 in Numerical Recipes, by
c  Press, Flannery, Teukolsky, and Vetterling, Cambridge Univ Press, 1986,
c  pp. 460-462.

c  the variable nequal was added to ensure convergence in the case where 1 or
c  more data values are equal to the current guess for the median (= a)

c  when more than one data value is equal to the true median and the current
c  guess (= a) is near but not equal to this value, then convergence can be
c  slowed by successive iterations that oscillate about the true median.
c  under these conditions, either xp is equal to xm from the previous iteration,c  or xm is equal xp from the previous iteration.  the new variables xplast and
c  xmlast are used to test for this case.

c  error return code ierr = 0 for no errors
c			  = maxit (= max(100,100*log(n)) if convergence does not
c				  occur within this number of iterations, in
c				  which case xmed is set to the current guess

	integer*4 n
	real*4 x(n)

	integer*4 np, nm, nequal, j, halfn
	real*4 xx, xp, xm, sumx, sum, eps, dum
	real*4 ap, am, aa, a

	parameter (big=1.0e30, afac=1.5, amp=1.5)

	ierr = 0

	a = 0.5*(x(1)+x(n))
	eps = abs(x(n)-x(1))
	am = -big
	ap = big
	halfn = n/2
	xplast = big
	xmlast = -big
	maxit = 100*nint(log(1.*n))
	if (maxit .lt. 100) maxit = 100
	nit = 0

1	nit = nit + 1
	if (nit .gt. maxit) then
	  ierr = maxit 
	  xmed = a
	  return
	endif
	sumx = 0.0
	sum = 0.0
	np = 0
	nm = 0
	nequal = 0
	xm = -big
	xp = big

	j = 0
	do while (j .lt. n)
	  j = j + 1
	  xx = x(j)

	  if (xx .eq. a) then
	    nequal = nequal + 1

	  else

	    if (xx .gt. a) then
	      np = np + 1
	      if (xx .lt. xp) xp = xx

	    else if (xx .lt. a) then
	      nm = nm + 1
	      if (xx .gt. xm) xm = xx
	    endif

	    dum = 1.0/(eps+abs(xx-a))
	    sum = sum + dum
	    sumx = sumx + xx*dum

	  endif

	enddo

c -- check for oscillations about true median

	if (xm .eq. xplast) then
	  a = xm
	  go to 1
	else if (xp .eq. xmlast) then
	  a = xp
	  go to 1
	else
	  xmlast = xm
	  xplast = xp
	endif

c -- adjust xp,np and xm,nm if some data values equal current guess

	if (nequal .gt. 0) then

	  if (np .lt. halfn) then
	    np = np + nequal
	    if (np .gt. halfn) np = halfn
	    xp = a
	  endif

	  if (nm .lt. halfn) then
	    nm = nm + nequal
	    if (nm .gt. halfn) nm = halfn
	    xm = a
	  endif

	endif

c -- check distribution of points about current guess

	if (np-nm .gt. 1) then
c					guess too low
	  am = a
	  aa = xp + max(0., sumx/sum-a)*amp
	  if (aa .gt. ap) aa = 0.5*(a+ap)
	  eps = afac*abs(aa-a)
	  a = aa
	  go to 1

	else if (nm-np .gt. 1) then
c					guess too high
	  ap = a
	  aa = xm + min(0., sumx/sum-a)*amp
	  if (aa .lt. am) aa = 0.5*(a+am)
	  eps = afac*abs(aa-a)
	  a = aa
	  go to 1

c -- success

	else

	  if (mod(n,2) .eq. 0) then
c					n is even
	    if (np .eq. nm) then
	      xmed = 0.5*(xp+xm)
	    else if (np .gt. nm) then
	      xmed = 0.5*(a+xp)
	    else
	      xmed = 0.5*(xm+a)
	    endif

	  else
c					n is odd
	    if (np .eq. nm) then
	      xmed = a
	    else if (np .gt. nm) then
	      xmed = xp
	    else
	      xmed = xm
	    endif

	  endif

	endif

	return

	end
