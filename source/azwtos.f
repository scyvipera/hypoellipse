c azwtos.for   []
      subroutine azwtos
c azimuthal weighting of stations by quadrants
      include 'params.inc'
      character*4 iahead*60, msta*5, nsta*5, icard*110
      common /char/ iahead, msta(npa), nsta(nsn), icard
      common /amo/ tempg(npa), ntgap
      common /gmost/ az(npa)
      common /pmost/ nr,nrp,lbastm,tp(npa),ksmp(npa),kdx(npa)
      common /qmost/ wt(npa),z
      dimension tx(4),txn(4),ktx(4),kemp(npa),key(npa)
c count and sort by azimuth stations with weight .ne. 0
      j=0
      do 10 i=1,nr
      if (wt(i) .eq. 0.) goto 10
      j=j+1
      tempg(j)=az(i)
   10 continue
c divide into 4 zones with one axis bisecting largest gap between stations
      call sort(tempg,key,j)
      gap=tempg(1)+360.-tempg(j)
      ig=1
      do 20 i=2,j
      dtemp=tempg(i)-tempg(i-1)
      if (dtemp .le. gap) goto 20
      gap=dtemp
      ig=i
   20 continue
      tx(1)=tempg(ig)-0.5*gap
      tx(2)=tx(1)+90.
      tx(3)=tx(1)+180.
      tx(4)=tx(1)+270.
      do  30 i=1,4
      txn(i)=0.
      if (tx(i) .lt. 0.) tx(i)=tx(i)+360.
      if (tx(i).gt.360.) tx(i)=tx(i)-360.
   30 continue
      call sort(tx,ktx,4)
      do  80 i=1,nr
      if (wt(i) .eq. 0.) goto  80
      if (az(i) .gt. tx(1)) goto  50
   40 txn(1)=txn(1)+1.
      kemp(i)=1
      goto  80
   50 if (az(i) .gt. tx(2)) goto  60
      txn(2)=txn(2)+1.
      kemp(i)=2
      goto  80
   60 if (az(i) .gt. tx(3)) goto  70
      txn(3)=txn(3)+1.
      kemp(i)=3
      goto  80
   70 if (az(i) .gt. tx(4)) goto  40
      txn(4)=txn(4)+1.
      kemp(i) = 4
   80 continue
      xn=4
      if (txn(1).eq.0.) xn=xn-1
      if (txn(2).eq.0.) xn=xn-1
      if (txn(3).eq.0.) xn=xn-1
      if (txn(4).eq.0.) xn=xn-1
      fj=j/xn
      do  90 i=1,nr
      if (wt(i) .eq. 0.) goto  90
      ki=kemp(i)
c new weight equals old weight times total number of stations
c divided by number of zones contatning stations and divided
c------- again by number of zones containing this station
      wt(i)=wt(i)*fj/txn(ki)
   90 continue
      return
      end
c end azwtos
