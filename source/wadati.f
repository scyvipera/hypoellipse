c wadati.for    []
      subroutine wadati(nr, nrp, w, ldx, tp, kdate, khrmn, iprn,
     *      ilis, krmp, krms, msta, test, vpvs3,  worig, se3)
c               original version written by w. gawthrop 197?.
c               modified and comments added by c. stephens, aug 1981.
c               modified for addition to hypoellipse and to account
c               for estimated reading errors by j. c. lahr, jan 1986.
c-------------------------------------------------------------------
c   this program is used to generate wadati (s-p interval vs
c   p-arrival time) plots on the line printer from hypoellipse phase
c   data.  the program determines the vp/vs ratio (slope of the ts
c   vs tp curve), the origin time (tp intercept) and standard error
c   of the slope.
c   the phase data from one or more earthquakes may be input.  a
c   separate plot will be made and parameters determined for each
c   event, and an average vp/vs for all of the events will be
c   determined.
c
c-------------------------------------------------------------------
      include 'params.inc' 
      character*4 isym(npa), msta(npa)*5
      character*4 krmp(npa), krms(npa)
      dimension w(npa), ldx(npa), tp(npa), key(npa)
      dimension test(100)
      dimension par(npa), pwt(npa), sar(npa), swt(npa)
      integer punt
      common /punt/ punt
c------- check for s readings
      if((nr-nrp) .ge. 3) goto 31
29      if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 30) nr, nrp
30      format(' too few s readings for wadati calculation:', /,
     *         '  of ', i5, ' readings, ', i5, ' are p.')
        vpvs3 = 0.
        return
31    continue
      pimax = 0.
c------- main loop to compute vp/vs and origin time
c assume s is dependent and p is independent variable
      nvpvs = 0
      do 40 i=1,nrp
c------- determine station plot symbol
c------- no s data
      if(ldx(i) .eq. 0) goto 40
c------- s weight = 0
      if(w(ldx(i)) .eq. 0.) goto 40
c------- p weight = 0
      if(w(i) .eq. 0.) goto 40
c------- found one with p and s data
      nvpvs = nvpvs + 1
      key(nvpvs) = i
      isym(nvpvs) = msta(i)(1:4)
      par(nvpvs) = tp(i)
      pwt(nvpvs) = w(i)
      if(par(nvpvs) .gt. pimax) pimax = par(nvpvs)
      sar(nvpvs) = tp(ldx(i))
      swt(nvpvs) = w(ldx(i))
40    continue
      if(nvpvs .lt. 3) goto 29
      call line3(nvpvs, par, sar, pwt, swt, test,
     *                         vpvs1, sint1, orig1, se1, ses, oses,
     *                         vpvs2, sint2, orig2, se2, sep, osep,
     *                         vpvs3, sint3, orig3, se3)
      khrmn1 = khrmn
      if(orig1 .lt. 0.) then
        khrmn1 = khrmn1 - 1
        orig1 = orig1 + 60.
      endif
      khrmn2 = khrmn
      if(orig2 .lt. 0.) then
        khrmn2 = khrmn2 - 1
        orig2 = orig2 + 60.
      endif
      khrmn3 = khrmn
      worig = orig3
      if(orig3 .lt. 0.) then
        khrmn3 = khrmn3 - 1
        orig3 = orig3 + 60.
      endif
      if(iprn .le. 0 .and. ilis .gt. 0) 
     *  write(punt, 41) vpvs3, se3, nvpvs
41    format (' bi-weight vp/vs = ', f8.3, ' +/- ', f5.3,
     * ' based on ', i5, ' stations with p and s')
      if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 42)
     *  vpvs1, se1, kdate, khrmn1, orig1, sint1, oses, ses,
     *  vpvs2, se2, kdate, khrmn2, orig2, sint2, osep, sep,
     *  vpvs3, se3, kdate, khrmn3, orig3, sint3
42    format (/,
     * '              vp/vs ratio    std. error of vp/vs  date ',
     *'  hrmn  origin   s-intercept  estimated std. er.  std. er. used',
     * /,
     * ' 1) s vs p   ', f12.4, f17.4, i13, i6, f8.2, f12.2, 8x,
     *                     f5.2, 13x, f5.2, /,
     * ' 2) p vs s   ', f12.4, f17.4, i13, i6, f8.2, f12.2, 8x,
     *                     f5.2, 13x, f5.2, /,
     * ' 3) bi-weight', f12.4, f17.4, i13, i6, f8.2, f12.2)
c------- write out phase data used in wadati calculation
      if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 20)
20    format (/,
     *' station  prmk  p-time    p-wt  srmk  s-time    s-wt  deviation',
     *' from line')
c      xxxaaaaxxxaaaaxxffffffxxffffffxxaaaaxxffffffxxffffff
      denom = sqrt(1. + vpvs3*vpvs3)
      do 60 i = 1, nvpvs
        j = key(i)
        dev = (sar(i) - sint3 - vpvs3*par(i))/denom
        if(iprn .ge. 1 .and. ilis .gt. 0)
     *   write(punt, 35) isym(i), krmp(j), par(i), pwt(i),
     *   krms(j), sar(i), swt(i), dev
35      format (      3x,a4, 3x,a4, 2x,f6.2, 2x,f6.2, 2x,a4, 2x,f6.2,
     *                  2x,f6.2, 9x,f6.2 )
60    continue
      if(abs(test(49)) .gt. 1.) then
c------- extend array for plotting fit lines
        do 62 i = 1, nvpvs
          par(i) = par(i) - worig
          sar(i) = sar(i) - worig
62      continue
        xaxmx = pimax - worig + 2.
        yaxmx = xaxmx*vpvs3
        par(nvpvs+1) = xaxmx - 1.
63      sar(nvpvs+1) = vpvs3*par(nvpvs+1)
        if(sar(nvpvs+1) .gt. yaxmx) then
          par(nvpvs+1) = par(nvpvs+1) - 1.
          goto 63
        endif
        isym(nvpvs+1) = '3'
        par(nvpvs+2) = 1.1
        sar(nvpvs+2) = vpvs3*par(nvpvs+2)
        isym(nvpvs+2) = '3'
        par(nvpvs+3) = xaxmx - 1.5
64      sar(nvpvs+3) = sint1 + worig*(vpvs1 - 1.) +
     *    vpvs1*par(nvpvs+3)
        if(sar(nvpvs+3) .gt. yaxmx) then
          par(nvpvs+3) = par(nvpvs+3) - 1.
          goto 64
        endif
        isym(nvpvs+3) = '1'
        par(nvpvs+4) = .8
        sar(nvpvs+4) = sint1 + worig*(vpvs1 - 1.) +
     *    vpvs1*par(nvpvs+4)
        isym(nvpvs+4) = '1'
        par(nvpvs+5) = xaxmx - 2.5
66      sar(nvpvs+5) = sint2 + worig*(vpvs2 - 1.) +
     *    vpvs2*par(nvpvs+5)
        if(sar(nvpvs+5) .gt. yaxmx) then
          par(nvpvs+5) = par(nvpvs+5) - 1.
          goto 66
        endif
        isym(nvpvs+5) = '2'
        par(nvpvs+6) = .5
        sar(nvpvs+6) = sint2 + worig*(vpvs2 - 1.) +
     *    vpvs2*par(nvpvs+6)
        isym(nvpvs+6) = '2'
        kk = nvpvs + 6
        if(iprn .ge. 1 .and. ilis .gt. 0) write(punt, 70) nvpvs
70      format(1x, 'wadati plot with ', i5, ' data points.')
c------- make wadati plot
cd      print *, 'par ', par
cd      print *, 'sar ', sar
cd      print *, 'isym ', isym
        if(iprn .ge. 0 .and. ilis .gt. 0)
     *  call prplot(par, sar, xaxmx,  0., yaxmx,  0.,   56,  kk,isym,
c                      x,  y,  xmax,xmin,  ymax,ymin,lines,last,isym,
     *     1,   1)
c         no,most)
      endif
      return
      end
c end wadati
