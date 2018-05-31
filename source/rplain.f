c rplain.for    []
      subroutine rplain
c set up one equation to constrain hypocenter to a plane
      save
      include 'params.inc' 
      parameter (pi = 3.1415926)
      parameter (rpd = pi/180.)
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /imost/ test(100)
      common /oqr/ y(4),kz,at(3),tl(3),pdrms,b(4)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /qmost/ wt(npa),z
      common /rioq/ been,damp,dmpinc,igo
      common /tmost/ x(4,npa)
      dimension hh(3)
c
c fix hypocenter on a plane.
      if (been .ne. 1.0) then
        been = 1.0
        az = abs(test(28))
        di = test(30)
        dir = di*rpd
        cir = pi/2.0-dir
        azr = az*rpd
        hh(1) =  cos(cir)*sin(azr)
        hh(2) = -cos(cir)*cos(azr)
        hh(3) =  sin(cir)
      endif
   75 nrm3 = nr
      wt(nrm3) = 1.
      ksmp(nrm3) = 0
      msta(nrm3) = 'fxpl '
      x(1,nrm3) = hh(1)*test(47)
      x(2,nrm3) = hh(2)*test(47)
      x(3,nrm3) = hh(3)*test(47)
      x(4,nrm3) = 0.0
      return
      end
c end rplain
