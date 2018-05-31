c uamag.for    [unix]
      subroutine uamag (icent2, msta, source, date, dstsqr,
     &  amplit, period, magnit, clusei, sysmag, gndmot,
     & wa_static_mag, punt, blank)
c originally written by steve a. estes long, long ago.
c file fields: msta, start date, stop date, mag for
c   periods .6, .5, .4, .3, .2, .1
c updated 10/22/81 by sae to increase deminsion of
c   f, lsta, date1, date2 to 500.
c updated 12/09/82 by ghcs to be compatible with hypoe82.
c updated 03/23/88 by jcl to incorporate into hypoellipse.
c   stations with system response code = 18 will use this
c   subroutine for xmag calc.
c re-written 09/21/88 by ghcs:  generalized the calibration data to
c   include an arbitrary number of period, magnification pairs for
c   each station entry.  the program was also restructured in fortran
c   77 and separate routines were created:  get_uacaldata() for input
c   and log_interp() for interpolation.
c once more 5/17/89 by jcl.
c   modified to be compatible with vax/vms fortran, which can not read
c   a free format character variable unless the variable is enclosed
c   in quotes.  format must now be a3 or a4 station name followed by
c   one space and then the a1 source code.
c   removed implicit undefined statements.
c   make all source comparisons in upper case.
c   corrected loga0_tbl(2,2) from 50 to 35
c updated 10/20/89 by ghcs to use unformatted data file via the
c   get_bin_uacaldata() routine.
c 6/6/91 by jcl
c   extended loga0_tbl down to 0.1 km distance, to make this routine
c   compatible with xfmags, and so that magnitudes can be computed at
c   close distances.
c 12/23/91 by jcl
c   msta and amp_source are lower case, so use dnstrn in comparison
c 7/15/95 correct interpolation period in wood_anderson table
c 10/22/95 eliminate wood_anderson interpolation and use function wa_magn
c 5/30/97 extented loga0_tbl to 1500 km distance.  jcl
c 2/24/99 add icent2 to keep track of century.  jcl

      save station_list, amp_source, nrecords, npoints,
     &     begin_date, end_date, period_sysmag, 
     &     loga0_tbl
      integer max_records, max_stacode, max_points, max_wapoints
      integer max_loga0pts
      integer punt
c* (vax
c* (unix
      parameter (max_records = 1000)
c* unix)
c* vax)
c* (pc
c      parameter (max_records = 100)
c* pc)
      parameter (max_stacode = 4)
      parameter (max_points = 30)
      parameter (max_wapoints = 4)
      parameter (max_loga0pts = 4)
      integer data_unit
      parameter (data_unit = 24)
c --- declare the subroutine arguments:
      character msta*(5)
      integer date
      real dstsqr, amplit, period, magnit, clusei
c --- declare the common variables:
      character*50 uacal
      common /dix/ iexcal, uacal
c --- declare the local variables:
      character*4 dnstrg
      character station_list(max_records)*(max_stacode)
      character amp_source(max_records)*1, source*1
      integer i, j, k, nrecords, npoints(max_records)
      integer begin_date(max_records), end_date(max_records)
      integer beg_cent, end_cent
      real period_sysmag(2,max_points,max_records)
c     real wood_anderson(2,max_wapoints)
      real loga0_tbl(2,max_loga0pts)
c     real zmc1, zmc2, pwc1, pwc2
      real loga, loga0, sysmag, wa_mag, km, slope, delta, x1, x2
c --- log-log interpolation function.
      real log_interp	
c --- are the periods here given in backwards order???
c     data wood_anderson/.1, 2787.5, .2, 2747.1, .3, 2671.5,
c    &                   .4, 2553.4, .5, 2391.0, .6, 2192.2/
c correct 3rd period 7/15/95 from .8 to .6
c    &                    .8, 2192.2, 42.4, 1.0/
c compute wood anderson magnification rather than extrapolate
c starting 10/22/95
c     data wood_anderson /.01, 2800., .1, 2787.5,
c    &                    .6, 2192.2, 42.4, 1.0/
c     data loga0_tbl /10.,1.5, 50.,2.3, 200.,3.5, 600.,4.9/
c     data loga0_tbl /10.,1.5, 35.,2.3, 200.,3.5, 600.,4.9/
c     data loga0_tbl /0.1,-1.75, 35.,2.3, 200.,3.5, 600.,4.9/
      data loga0_tbl /0.1,-1.75, 35.,2.3, 200.,3.5, 1500.,6.15/
c     data zmc1/0.15/, zmc2/3.38/, pwc1/0.80/, pwc2/1.50/
      data nrecords/-1/
c     write (punt,*) 
c    & 'msta, source, date, dstsqr, amplit, period, magnit, clusei'
c     write (punt,*) msta, source, date, dstsqr,
c    &                    amplit, period, magnit, clusei
c a source of '%' is presumed to be the same as 'a'
      sysmag = blank
      gndmot = blank
      if (source .eq. '%') source = 'a'
c **********************************************
c *** correction for magnification different ***
c *** from 2800.                             ***
c     amag_cor = wamag/2800.
c ***                                        ***
c **********************************************
c *** the first time the routine is called, ***
c *** read in the initial calibration data. ***
c *** Units of sysmag are "units"/mm.       ***
c *** Currently (1991) units are counts/mm  ***
c *********************************************
      if (nrecords.eq.-1)  then
cd        print *, 'about to open ', uacal
cd        print *, 'for unformatted read on unit ', data_unit
c* (unix
        call openfl(data_unit, uacal, 'old', 'null', 'none',
     *    'unformatted', 0)
        print *, 'about to call getbin to read unit ', data_unit
        call getbin (data_unit,
     &  max_records, max_points, nrecords, station_list, amp_source,
     &  begin_date, end_date, npoints, period_sysmag)
c* unix)
c* (vax
c* (pc
c        call get_uacaldata (uacal, max_records,
c     &  max_points, nrecords, station_list, amp_source, begin_date,
c     &  end_date, npoints, period_sysmag, data_unit, punt)
c* pc)
c* vax)
        print *, 'just read ', nrecords, ' records'
        close (data_unit)
      endif
c ***********************************************
c *** convert the distance squared into km.   ***
c *** also, we only want to calculate a mag   ***
c *** for dstsqrs defined in the loga0_tbl    ***
c *** & periods defined in the wood anderson  ***
c *** table.                                  ***
c ***********************************************
      km = sqrt(dstsqr)
c     if (km.lt.loga0_tbl(1,1) .or.
c    &    km.gt.loga0_tbl(1,max_loga0pts)) then
c       clusei = -1
c       return
c     end if
c     if (period.lt.wood_anderson(1,1) .or.
c    &    period.gt.wood_anderson(1,max_wapoints) ) then
      if (period .le. 0.0) then
        clusei = -1
        return
      end if
c *******************************************************
c *** find this station's current calibration record. ***
c *** this should be done much more efficiently!      ***
c *******************************************************
      do 50 k=1, nrecords
c      print *, 'station = ', dnstrg(station_list(k)), msta 
c      print *, 'source = ', dnstrg(amp_source(k)), source
        if (end_date(k) .eq. 999999) end_date(k) = 699999
        if ((dnstrg(station_list(k)) .eq. msta(1:4)) .and.
     &      (dnstrg(amp_source(k))   .eq. source)) then
c if the year is less than 70, then this must be after 1999
	  if (begin_date(k)/10000 .lt. 70) then
	     beg_cent = 20
	  else
	     beg_cent = 19
	  endif
	  if (end_date(k)/10000 .lt. 70) then
	     end_cent = 20
	  else
	     end_cent = 19
	  endif
          if (  (
     *            ((beg_cent .eq. icent2) .and. 
     *	           (begin_date(k) .le. date)) .or.
     *	          (beg_cent .lt. icent2)
     *          ) 
     *          .and.
     *          (  
     *            ((end_cent .eq. icent2) .and.
     *             (end_date(k) .ge. date)) .or.
     *            (end_cent .gt. icent2)
     *          )
     *       ) goto 100
        endif
50    continue
c --- no calibration data matches
      write (punt,'('' no calibration data for /'',a4,''/ source='',a1,
     &  '' date='',i2.2,i6.6)') msta(1:4), source, icent2, date
      clusei = -1.0
      return	
c ***********************************************************
c *** find the system magnification for the given period. ***
c ***********************************************************
100   if (period.lt.period_sysmag(1,1,k) .or.
     &    period.gt.period_sysmag(1,npoints(k),k) ) then
        write (punt,
     &    '('' no calibration data for /'',a4,''/ source='',a1,
     &    '' date='',i6.6,i2,'' at period='',f5.2)')
     &    msta(1:4), source, date, icent2, period
        clusei = -1
        return
      end if
      do 120 i=1, npoints(k)-1
        j = i + 1
        if (period.eq.period_sysmag(1,i,k)) then
c         we have an exact period match, so don't interpolate
          sysmag = period_sysmag(2,i,k)
          goto 200
        else if (period.eq.period_sysmag(1,j,k)) then
c         we have an exact period match, so don't interpolate
          sysmag = period_sysmag(2,j,k)
          goto 200
        else if (period.gt.period_sysmag(1,i,k) .and.
     &           period.lt.period_sysmag(1,j,k)) then
c         we have a period within our defined range, so interpolate.
          sysmag = log_interp (period,
     &               period_sysmag(1,i,k), period_sysmag(2,i,k),
     &               period_sysmag(1,j,k), period_sysmag(2,j,k))
          if (sysmag.lt.0.0) then
            write (punt,'('' sysmag.lt.0 for '',a4)') msta(1:4)
            clusei = -1.0
            return
          endif
          goto 200
        end if
120   continue
c --- we should never be able to get here.
      clusei = -1
      return
c *************************************************************
c *** find the wood anderson response for the given period. ***
c *************************************************************
200   wa_mag = wa_magn(wa_static_mag, period)
c ***************************************
c *** bail out if sysmag is too small ***
c ***************************************
      if (sysmag.lt.0.00001) then
        write (punt,'('' sysmag.lt.0.00001 for '',a4)') msta(1:4)
   	sysmag = blank
        clusei = -1.0
        return
      endif
c *************************
c * compute ground motion *
c *************************
      gndmot = (amplit*10**3)/sysmag
c **********************************
c * find the correct distance term *
c **********************************
      do 320 i=1, max_loga0pts-1
        j = i + 1
        if (km.eq.loga0_tbl(1,i)) then
          loga0 = loga0_tbl(2,i)
          goto 400
        else if (km.eq.loga0_tbl(1,j)) then
          loga0 = loga0_tbl(2,j)
          goto 400
        else if (km.gt.loga0_tbl(1,i) .and. km.lt.loga0_tbl(1,j)) then
c ---     interpolate
          x1 = log10(loga0_tbl(1,i))
          x2 = log10(loga0_tbl(1,j))
          delta = x2 - x1
          if (abs(delta) .le. 0.0000001) then
            loga0 = loga0_tbl(2,i)
          else
            slope = (loga0_tbl(2,j) - loga0_tbl(2,i)) / delta
            loga0 = slope * (log10(km) - x1) + loga0_tbl(2,i)
          endif
          goto 400
        end if
320   continue
c --- we shouldn't be able to get here since
c --- outlaying distances were trapped above.
      clusei = -1
      return
c *****************************
c *** compute the magnitude ***
c *****************************
400   loga = log10( amplit * wa_mag / (2. * sysmag) )
c     loga0 = zmc1 - pwc1 * log10(km)
c     if (km.ge.40.) loga0 = zmc2 - pwc2 * log10(km)
      magnit = loga + loga0
c     write (punt,*) ' amp=',amplit,' period=',period,
c    &  ' dist_km=',km,' sysmag=',sysmag,' wa_mag=',wa_mag,
c    &  ' loga=',loga,' loga0=',loga0,' mag=',magnit
      clusei = sysmag / 1000000.
c *****************************************************
c *** exclude magnitude if distance is out of range ***
c *****************************************************
      if (km.lt.loga0_tbl(1,1) .or.
     &    km.gt.loga0_tbl(1,max_loga0pts)) then
        clusei = -1
      end if
      return
      end
c *************************************************************
c *************************************************************
c* (vax
c* (pc
c      subroutine get_uacaldata (filename, max_records, max_points,
c     &             nrecords, station_list, amp_source, begin_date,
c     &             end_date, npoints, period_sysmag, data_unit,punt)
cc           - subroutine get_uacaldata is not used on unix systems.
cc             however, please leave code here so that switch to vax
cc             can be made.
c* pc)
c* vax)
c* (pc
c$notruncate
c* pc)
c* (vax
c* (pc
cc --- input for this routine is a free formatted ascii file of
cc --- station calibration data.  it is a sequence of up to
cc --- <max_records> (logical) records (which may span several
cc --- physical records) ordered in the following manner:
cc --- 'sta_code', 'source', begin_yymmdd, end_yymmdd, npairs,
cc --- period(1), sysmag(1), period(2), sysmag(2), ...
cc --- period(npairs), sysmag(npairs)
cc --- note: the period, magnification pairs must be in order of
cc --- increasing period (i.e. this is reversed from the old routine).
c      character*300 calrec
c      character rshft*4, dnstrg*4
c      integer max_records, max_points, nrecords
c      character filename*(*), station_list(max_records)*(*)
c      character amp_source(max_records)*1
c      integer  begin_date(*), end_date(*), npoints(*), data_unit
c      real period_sysmag(2,max_points,max_records)
c      integer i, punt
c      call openfl(data_unit, filename, 'old', 'null', 'none',
c     *    'none', 0)
c      nrecords = 0
c100   if (nrecords.lt.max_records) then
c        nrecords = nrecords + 1
c        read (data_unit, '(a)', end=116) calrec
cc because standard fortran 77 to can not read an internal file with
cc free format, file 14 must be used in the following code!
c        rewind 14
c        write(14, '(a)') calrec
c        station_list(nrecords) = calrec(1:4)
c        rewind 14
c        if(calrec(4:4) .eq. ' ') then
c          amp_source(nrecords) = dnstrg(calrec(5:5))
c          write(14, '(a)') calrec(6:lentru(calrec))
c        else
c          amp_source(nrecords) = dnstrg(calrec(6:6))
c          write(14, '(a)') calrec(7:lentru(calrec))
c        endif
c        rewind 14
c        read (14, *, err=118)
c     &    begin_date(nrecords), end_date(nrecords), npoints(nrecords),
c     &    (period_sysmag(1,i,nrecords), period_sysmag(2,i,nrecords),
c     &    i=1, npoints(nrecords))
cc ---   this doesn't solve the overflow problem, but it might help.
c        if (npoints(nrecords).gt.max_points) then
c          npoints(nrecords) = max_points
c          write (*, '('' warning: too many data points on record #'',
c     &      i6, '' of uofa calibration file:'', /, a)')
c     &      nrecords, filename
c        endif
cc ---   right justify the 3-character station codes and shift to uppercase.
c        station_list(nrecords) = dnstrg(rshft(station_list(nrecords)))
cc ---   read next record
c        goto 100			
c116     continue
c          nrecords = nrecords - 1	
c          return
c118     continue
c          write (punt, 120) nrecords, filename
c120       format(' warning: error reading record #', i5,
c     &           ' from uofa calibration file:', /, 1x, a, /,
c     &           ' remainder of file skipped.')
c          return
c      else
c        write (punt, 130) max_records, filename
c130     format(' warning: too many records (>',i5,
c     &    ') in uofa calibration file:', /, 1x, a)
c      endif
c      return
c      end
c* pc)
c* vax)
cc *************************************************************
      real function log_interp (x, x1, y1, x2, y2)
c* (pc
c$notruncate
c* pc)
c --- returns the y cooresponding to a linear interpolation
c --- between x1, y1 and x2, y2 in the log-log plane.
c --- note that -1.0 is returned if any of the arguments are
c --- not positive. y1 is returned if the difference in the
c --- logs of x1 and x2 is too near zero.
      real x, x1, x2, y1, y2
      real log_x, log_x1, log_x2, log_y1, log_y2
      real delta, slope
      if ((x.le.0.0) .or.
     &    (x1.le.0.0) .or. (x2.le.0.0) .or.
     &    (y1.le.0.0) .or. (y2.le.0.0) ) then
c       write (punt,*) ' error', x, x1, y1, x2, y2
c ---   error, return error status
        log_interp = -1.0	
      else
        log_x = log10(x)
        log_x1 = log10(x1)
        log_x2 = log10(x2)
        log_y1 = log10(y1)
        log_y2 = log10(y2)
        delta = log_x2 - log_x1
        if (abs(delta) .le. 0.0000001) then
c ---     avoid dividing by zero
          log_interp = y1	
        else
          slope = (log_y2 - log_y1) / delta
          log_interp = 10.0 ** (slope * (log_x - log_x1) + log_y1)
        endif
      endif
      return
      end
c end uamag
