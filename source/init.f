c init.for    [unix]
      subroutine init
c* (pc
c$notruncate
c* pc)
c get filenames, open files and write greeting for hypoellipse
      include 'params.inc' 
      character*256 root
      common /dhip/ inpt,isa,ilis,inmain,injump
      character*4 ipro, ichec, evtype*1, evstat*1
      common /dbhip/ ipro, ichec, evtype, evstat, root
      common /hf/ cp(72),sp(72)
      real*8 time1, time2
      common /hop/ time1,time2,nopu,notim
      common /it/ vpvs(lmax2),vpvsm(mmax+3),nttab
      common /lm/ mapend
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      integer punt
      common /punt/ punt
      integer print_unit, summary_unit, arch_unit
      parameter (print_unit   = 9)
      parameter (summary_unit = 4)
      parameter (arch_unit    = 11)
      character*1 print_type, summary_type, arch_type
      parameter (print_type   = 'o')
      parameter (summary_type = 's')
      parameter (arch_type    = 'a')
      character openstat*7
      openstat = 'unknown'
c
c open the files for hypoellipse (returns root name)
        call opfls(inpt, inmain, root, nttab)
        call timit(0)
c get current date
        call jdate(irmo, irdy, iryr, ihr, imn, isec)
c do not print output conversion errors
        call erset (63,.true.,.false.,.false.,.false.)
        write(punt, 20) nsn, npa, iryr, irmo, irdy, ihr, imn
20      format (
c* (pc
c     *' *** Hypoellipse: PC/Non-Xpick/Y2K version 3.9 11/1/2001  ***',
c* pc)
c* (unix
     *' *** Hypoellipse: Unix/Non-Xpick/Y2K version 3.9 11/1/2001  ***',
c* unix)
     */, '  Configured for up to ', i4, ' stations in station list',
     */, '  and up to ', i4, ' records per earthquake.',
     */, '  Run on ', 2(i2.2, '/'), i2.2, ' at ', i2.2, ':', i2.2)
c calculate tables to be used in focal mechanism plots
        do 30 i=1,72
c         pi=i*.0349066*2.5
          pi = i*0.0872665
          cp(i)=cos(pi)
          sp(i)=sin(pi)
30      continue
        ichec = 't s '
        mapend=0
        nr = 0
        time1 = 0.0d0
      return
      end
c end initial
