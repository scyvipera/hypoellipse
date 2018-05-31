c cyldly.for    []
      subroutine cyldly
     *  (kno, alat, alon, lat1, lon1, x0, y0, z, dly, sdly, iprn,
     *   modset, test8)
      save cyldy, cylrd, cylrd1, cylup, cylup1, cyldn, cyldn1,
     *  xc, yc, ncyl, setcyl
      real lat1, lon1
      character*1 ins, iew
      include 'params.inc' 
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      parameter (mxcyl = 50)
      logical setcyl
      real     cyld
c              cyld		horizontal distance to inner cylinder
      real     cyldin(mxcyl)
c              cyldin(i)    	distance to cylinder center for eq within
c                               inner radius
      integer  cyldy(mxcyl)
c              cyldy(i)		delay assigned to cylinder i
      integer  cylmd(mxcyl)
c              cylmd(i)		velocity model assigned to cylinder i
      real     cylrd(mxcyl)
c              cylrd(i)         inner radius of cylinder number i
      real     cylrd1(mxcyl)
c              cylrd1(i)        outer radius of cylinder number i
      integer  cyldm(mxcyl)
c              cyldm(i)		delay model for transition table entry i
      integer  cyldmin(mxcyl)
c              cyldmin(i)	delay model for inner table entry i
      real     cylup(mxcyl)
c              cylup(i)         upper limit of inner cylinder i
      real     cylup1(mxcyl)
c              cylup1(i)        upper limit of outer cylinder i
      real     cyldn(mxcyl)
c              cyldn(i)         lower limit of inner cylinder i
      real     cyldn1(mxcyl)
c              cyldn1(i)        lower limit of outer cylinder i
      real     cylwt(mxcyl)
c              cylwt(i)         weight derived for transition zone i
      real     dly(ndly,nsn)
c              dly(i, j)         p-delay for model i, station j
      integer  ntrans
c              ntrans		number of entries in transition table
      real     sdly(ndly,nsn)
c              sdly(i, j)        s-delay for model i, station j
      real     xc(mxcyl)
c              xc(i)            x coordinate of center of cylinder i
      real     yc(mxcyl)
c              yc(i)            y coordinate of center of cylinder i
      real     z
c              z		current trial eq depth
      real     zmarg
c              zmarg		vertical distance from event to endcap
c                               (negative for lower endcap)
      data setcyl /.false./
      modset = 0
      if(iprn .ge. 5) then
        print *, ' '
        print *, 'begin sub. cyldly to select delay and velocity'
	print *, 'model(s) based on cylindrical domains.'
      endif
      if (.not. setcyl) then
cd       print *, 'read in cylinder definitions with cylget'
        call cylget
     *  ( mxcyl, cyldy, cylmd, cylrd, cylrd1, cylup, cylup1, cyldn, 
     *    cyldn1, xc, yc, ncyl, lat1, lon1, test8)
        setcyl = .true.
      endif
c compute the x,y coordinates of the current trial epicenter
      call delaz(lat1, lon1, delt, deldeg, azz, alat, alon)
      x0 = delt*sin(azz*rad)
      y0 = delt*cos(azz*rad)
      if(iprn .ge. 5) then
	call unfold2(alat, alon, la, ins, ala, lo, iew, alo)
	print *, 'current epicenter = ', la, ins, ala, lo, iew, alo
        print *, 'location of epicenter wrt reference station is'
        print *, 'azimuth (deg) = ', azz
        print *, 'distance (km) = ', delt
        print *, 'x, y = ', x0, y0
        print *, 'loop through ', ncyl, ' regions. regions must be in '
        print *, 'order of preference in cases of overlap.'
      endif
      ntrans = 0
      ninner = 0
      do 20 i = 1, ncyl
        cyld = sqrt((x0 - xc(i))**2 + (y0 - yc(i))**2)
         if(iprn .ge. 5) then
	   print *, 'x, y of cylinder ', i, ' is ', xc(i), yc(i)
           print *, 'dist to this cylinder ',
     *     'which uses delay model ', cyldy(i), ' is ', cyld
           print *, 'z = ', z, ' cylup,dn(i) = ', cylup(i), cyldn(i)
        endif
	if ((cyld .le. cylrd(i)) .and. (z .ge. cylup(i)) .and.
     *    (z .le. cyldn(i))) then
          if(iprn .ge. 5) 
     *      print *, 'location is within inner cylinder'
	  ninner = ninner + 1
	  cyldin(ninner) = cyld
	  cyldmin(ninner) = i
        else if ((cyld .le. cylrd1(i)) .and. (z .ge. cylup1(1)) .and.
     *    (z .le. cyldn1(i))) then
c
cd         print *, 'location is within transition zone, so compute '
cd         print *, 'weight and add to list'
          ntrans = ntrans + 1
          cyldm(ntrans) = cyldy(i)
cd         print *, 'zmarg is distance into upper or lower cap'
          zmarg = 0.0
          if ((z .lt. cylup(i)) .and. (z .gt. cylup1(i)))
     *    zmarg = cylup(i) - z
          if ((z .gt. cyldn(i)) .and. (z .lt. cyldn1(i)))
     *    zmarg = cyldn(i) - z
cd         print *, 'note that zmarg will be negative for lower ',
cd    *             'cap region'
          if (zmarg .eq. 0.0) then
c
cd           print *, 'adjacent to cylinder '
cd           print *, 'cyld - cylrd(i) ', cyld - cylrd(i)
cd           print *, 'pi ', pi
cd           print *, 'cylrd1(i) - cylrd(i) ', cylrd1(i) - cylrd(i)
            cylwt(ntrans) = 0.5 + 0.5*cos( pi*(cyld - cylrd(i)) /
     *      (cylrd1(i) - cylrd(i)) )
          else if (cyld .le. cylrd(i)) then
c
cd           print *, 'within end caps'
            if (zmarg .gt. 0) then
cd             print *, 'within upper cap'
              cylwt(ntrans) = 0.5 + 0.5*cos( pi*zmarg /
     *        (cylup(i) - cylup1(i)) )
            else
c
cd             print *, 'within lower cap'
              cylwt(ntrans) = 0.5 + 0.5*cos( pi*zmarg /
     *        (cyldn(i) - cyldn1(i)) )
            endif
          else
c
cd           print *, 'within corner zone'
            if (zmarg .gt. 0) then
cd             print *, 'within upper corner zone'
              cylwt(ntrans) = 0.5 + 0.5*cos(pi*sqrt
     *        ( ((cyld - cylrd(i)) /
     *          (cylrd1(i) - cylrd(i)))**2 +
     *          (zmarg /
     *          (cylup(i) - cylup1(i)))**2 ) )
            else
cd             print *, 'within lower corner zone'
              cylwt(ntrans) = 0.5 + 0.5*cos(pi*sqrt
     *        ( ((cyld - cylrd(i)) /
     *          (cylrd1(i) - cylrd(i)))**2 +
     *          (zmarg /
     *          (cyldn(i) - cyldn1(i)))**2 ) )
            endif
          endif
        endif
20    continue

c use the parameters for the cylinder for which the eq is
c closest to the center
      if (ninner .ne. 0) then
	if(ninner .eq. 1) then
          kno = cyldy(cyldmin(ninner))
 	  modset = cylmd(cyldmin(ninner))
          return
        else
	  ntouse = 1
	  do 22 i = 2, ninner
	    if(cyldin(i) .lt. cyldin(ntouse)) then
	      ntouse = i
	    endif
22        continue
          kno = cyldy(cyldmin(ntouse))
 	  modset = cylmd(cyldmin(ntouse))
          return
	endif
      endif
	    
	  
      if (ntrans .eq. 0) then
cd       print *, 'not within any cylinders, so use default model (1)'
        kno = 1
        return
      else
cd       print *, 'weights: ', (cylwt(i), i = 1, ntrans)
cd       print *, 'models:  ', (cyldm(i), i = 1, ntrans)
c reduce list if there are 2 or more entries
        if (ntrans .gt. 1) call cylred(cyldm, cylwt, ntrans, sumwt)
        kno = ndly
c
        if (ntrans .eq. 1) then
          if (cylwt(1) .ge. 1.0) then
            kno = cyldm(1)
            return
          else
c           fill in with default model
            ntrans = 2
            cylwt(2) = 1.0 - cylwt(1)
            cyldm(2) = 1
            if (iprn .ge. 5) then
              print *, 'weights: ', (cylwt(i), i = 1, ntrans)
              print *, 'models:  ', (cyldm(i), i = 1, ntrans)
            endif
          endif
c
        else if (ntrans .eq. 3) then
          cylwt(1) = cylwt(1)/sumwt
          cylwt(2) = cylwt(2)/sumwt
          cylwt(3) = cylwt(3)/sumwt
          sumwt = 1.0
cd         print *, 'weights: ', (cylwt(i), i = 1, ntrans)
cd         print *, 'models:  ', (cyldm(i), i = 1, ntrans)
        else
c
c         ntrans = 2
          if (sumwt .gt. 1.0) then
            cylwt(1) = cylwt(1)/sumwt
            cylwt(2) = cylwt(2)/sumwt
          else
c           fill in with default model
            ntrans = 3
            cylwt(3) = 1. - sumwt
            sumwt = 1.0
            cyldm(3) = 1
cd           print *, 'weights: ', (cylwt(i), i = 1, ntrans)
cd           print *, 'models:  ', (cyldm(i), i = 1, ntrans)
          endif
        endif
      endif
c compute combined delays
      do 30 i = 1, nsn
        dly(ndly,i) = 0.0
        sdly(ndly,i) = 0.0
        do 28 j = 1, ntrans
          dly(ndly,i) =   dly(ndly,i) +  dly(cyldm(j), i)*cylwt(j)
          sdly(ndly,i) = sdly(ndly,i) + sdly(cyldm(j), i)*cylwt(j)
28      continue
30    continue
      return
      end
c end cyldly
