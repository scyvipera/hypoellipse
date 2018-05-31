c glob_new.for    []
      subroutine glob_new
c this subroutine attempt to find the global minimum when there is
c more than one local minimum at different depths.  j. c. lahr 4/22/87
c
      	include 'params.inc' 
      	parameter (ndly = 11)
      	integer punt
      	common /punt/ punt
      	common /gg/ altla(3), altlo(3), altz(3), altrms(3), nsolut,
     * fla(20), flo(20), fz(20), frms(20), forg(20), maxf, altorg(3)
      	logical ingulf
      	common /gu/ ingulf
      	logical freor
      	common /hopq/ savla,savlo,savez,savor,freor
      	common /igl/ nglobalzs, globalzs(20)
      	common /imost/ test(100)
      	common /ihfgpq/ itest(100)
      	common /phoqn/ inst,knst
      	common /qmost/ wt(npa),z
      	real latep,lonep
      	common /qmost1/ lonep,ni,latep
      	common /ro/ yse,seorg,phi
      	common /zmost/ nrwt,rms,nswt,avr,aar,nsmp
      	common /zqr/ fno

c
c deep depth should be deep enough to catch deepest local mimima.
c we use 75. km in alaska.  add test(8) so that deepz is in km wrt
c the top of the model, not wrt sea level.

      	i = nglobalzs
	
20	savez = globalzs(i) + test(8)
	write(punt, *) 'savez =', savez
      	instsv = inst
      	if(instsv .eq. 8) freor = .false.
c set inst = 1 to fix depth
      	inst = 1
      	call quakes

c free up the depth
	inst = 0
      	savla = latep
      	savlo = lonep
	savez = z
	write(punt, *) 'savez for free run is ', savez
	call quakes

	write(punt, *) 'z =', z

c save this depth and rms
      	fz(i) = z
      	fla(i) = latep
      	flo(i) = lonep
        frms(i) = rms
c     	rmslm1 = frms(1)
c     	if(fno .gt. 0.) rmslm1 = sqrt(frms(1)**2 + (yse**2)/fno)

c for now, just force a start at every depth specified
	if (i .gt. 1) then
	  i = i - 1
	  goto 20
	endif

c this version would allow depths to be skipped
c	if (i .gt. 1) then
c	  find next depth to test
c	  do j = i - 1, 1, -1
c	    if (fz(i) .gt. globalzs(j)) then
c	      i = j
c	      goto 20
c	    else
c	      skip this starting depth
c	      frms(j) = 999999.
c	      fz(j) = -99.
c	    endif
c	  enddo
c	endif

c now decide which was the best solution
	write(punt, *) frms
	write(punt, *) fz
	bestrms = frms(1)
	ibest = 1
	do i = 2, nglobalzs
	  if (frms(i) .lt. bestrms) then
	    bestrms = frms(i)
	    ibest = i
	  endif
	enddo

c get ready to make final run again
	savela = fla(ibest)
	savelo = flo(ibest)
	savez = fz(ibest)
	inst = instsv
	return 
	end

