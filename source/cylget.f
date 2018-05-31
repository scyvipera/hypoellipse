c cylget.for    []
      subroutine cylget
     * ( mxcyl, cyldy, cylmd, cylrd, cylrd1, cylup, cylup1, cyldn, 
     *   cyldn1, xc, yc, ncyl, lat1, lon1, test8 )
      real lat1, lon1
      parameter (ndly = 11)
      parameter (pi = 3.14159265)
      parameter (rad = pi/180.)
      character*1 ins, iew
      character*110 record
c read in the cylinders defining weight model regions
      integer  cyldy(mxcyl)
c              cyldy(i)		delay assigned to cylinder i
      integer  cylmd(mxcyl)
c              cylmd(i)         velocity model assigned to cylinder i
      real     cylrd(mxcyl)
c              cylrd(i)         inner radius of cylinder number i
      real     cylrd1(mxcyl)
c              cylrd1(i)        outer radius of cylinder number i
      real     cylup(mxcyl)
c              cylup(i)         upper limit of inner cylinder i
      real     cylup1(mxcyl)
c              cylup1(i)        upper limit of outer cylinder i
      real     cyldn(mxcyl)
c              cyldn(i)         lower limit of inner cylinder i
      real     cyldn1(mxcyl)
c              cyldn1(i)        lower limit of outer cylinder i
      real     xc(mxcyl)
c              xc(i)            x coordinate of center of cylinder i
      real     yc(mxcyl)
c              yc(i)            y coordinate of center of cylinder i
      ncyl = 0
cd     print *, 'read cylinder parameters'
      do 20 i = 1, mxcyl
18      read(17, '(a)', end = 90) record
        if(record(1:2) .eq. 'c*') goto 18
c because standard fortran 77 to can not read an internal file with
c free format, file 14 must be used in the following code!
        rewind 14
        write(14, '(a)') record
        rewind 14
c       read(record, *)
        read(14, *)
     *  cyldy(i),
     *  cylmd(i), blat, blon, cylrd(i), cylrd1(i), cylup(i),
     *  cylup1(i), cyldn(i), cyldn1(i)

c       adjust for the depth of the top of the model
        cylup(i) = cylup(i) + test8
        cylup1(i) = cylup1(i) + test8
        cyldn(i) = cyldn(i) + test8
        cyldn1(i) = cyldn1(i) + test8

c       compute the x,y coordinates of the cylinder center
        nlat = abs(blat)
        rlat = (abs(blat) - nlat)*60.
        ins = 'n'
        if (blat .lt. 0) ins = 's'
        nlon = abs(blon)
        rlon = (abs(blon) - nlon)*60.
        iew = 'w'
        if (blon .lt. 0) iew = 'e'
c convert to geocentric coordinates> alat, alon
cd       print *, 'blat, blon = ', blat, blon
cd       print *, 'nlat, ins, rlat, nlon, iew, rlon'
cd       print *, nlat, ins, rlat, nlon, iew, rlon
        call fold2(alat, alon, nlat, ins, rlat, nlon, iew, rlon)
        call delaz(lat1, lon1, delt, deldeg, azz, alat, alon)
cd       print *, 'location of cylinder wrt reference station is'
cd       print *, 'cyl lat lon = ', alat, alon
cd 	 print *, 'ref sta lat lon = ', lat1, lon1
cd       print *, 'azimuth (deg) = ', azz
cd       print *, 'distance (km) = ', delt
        xc(i) = delt*sin(azz*rad)
        yc(i) = delt*cos(azz*rad)
cd       print *, 'converted to x, y = ', xc(i), yc(i)
cd       print *, 'i, cyldy(i), cylmd(i), cylrd(i), cylrd1(i),', 
cd    *  'cylup(i), cylup1(i), cyldn(i), cyldn1(i)'
cd       print *,  i, cyldy(i), cylmd(i), cylrd(i), cylrd1(i), 
cd    *  cylup(i), cylup1(i), cyldn(i), cyldn1(i)
        if (cyldy(i) .gt. ndly) then
          print *, 'sub. cylget attempting to assign delay model',
     *    cyldy(i)
          print *, 'but only ', ndly, ' are allowed.'
          stop
        endif
20    continue
      ncyl = mxcyl
      return
90    ncyl = i - 1
      close (17)
      return
      end
c end cylget
