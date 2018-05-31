c--  ttgen
c--program to generate a table of travel times as a function of dist and depth
c--common variables for travel time table generating program ttgen
c-ttgen is a modified version of fred klein's program that computes a
c-travel time table for use with hypoellipse.
c-see the hypoinverse write up for details of the input.
c-some lines, as noted below, are now free format.

c-sample input file for ttgen:

c-                            format notes

c-linear1 tt032    .12   
c-  .05  100  .4   100        free format
c-  4.    12  10.   15        free format
c-  4.    26  15.   15        free format
c-bz linear model 
c-  5.6  0.0                  free format
c-  7.8  29.                  free format
c-  9.0  150.                 free format

	parameter (maxz=28,maxd=42,maxl=20,maxj=201)
c changed maxl from 15 to 20, February, 2000
	character ititl*20,ivm*8
	common /ttc/ ititl,ivm
	common /ttcom/ z(maxl),v(maxl),g(maxl),thk(maxl),vr(maxl)
	common /ttcom/ tp(maxj),tr(maxj),dp(maxj),dmin,tmin
	common /ttcom/ dd1,nd1,dd2,nd2,lay,zh,vh,n,ja(10),jb(10),jc,jd,jh
	common /ttcom/ ibr,tmax,dmax,dq1,nq1,dq2,nq2,pi2,rdeg,vm
	common /ttcom/ linp,lprt,lout,redv,jl,vjl,vjc
	common /ttcom/ dz1,nz1,dz2,nz2,nz,nd,ihd,ipa

	character*8 ipr, fname*50, aask*50

c--read i/o filenames and model parameters on ttmod
        fname = aask('name of file that specifies velocity model',
     *  'model.def', -50)
	open (unit=linp,file=fname,form='formatted',status='old')
	read (linp,1000) ipr,ivm,redv,vpvs
1000	format (2a8, 2f10.2)

c--read indexing parameters for depth and distance
	read (linp,*) dq1,nq1,dq2,nq2,dz1,nz1,dz2,nz2,dd1,nd1,dd2,nd2
c1001	format (f5.2,i5,f5.2,i5)

c--read distance limit, time limit, and title
	read (linp,1017) ititl
1017	format (a20)

c--open output files
	open (unit=lprt,file=ipr,form='formatted',status='new')
	open (unit=lout,file=ivm,form='formatted',status='new')
	write (lprt,1018) ititl
1018	format ('1  ', a20/)

c--read velocity model
	do i=1,maxl
	  read (linp,*,end=7) v(i),z(i)
	  if (v(i).le.0.) then
	    write (5,1004) i
	    write (lprt,1004) i
1004	    format (' error: velocity in layer',i3, ' must be (+)')
	    call exit(0)
	  end if
	  lay=i
	enddo
7	z(1)=0.
	close (linp)

c--define a fictitious second layer if halfspace model specified
	if (lay.le.1) then
	  v(2)=v(1)
	  z(2)=2.
	  lay=2
	end if
c--calc thickness and gradient for each layer and print the model
	write (lprt,1003) lay
1003	format (' velocity model with',i3,' layers:'//
	2 '  l  vel  depth thick   grad')
	do i=1,lay-1
	  thk(i)=z(i+1)-z(i)
	  if (thk(i).le.0.) then
	    write (5,1005) i
	    write (lprt,1005) i
1005	    format (' error: layer',i3,' must have (+) thickness')
	    call exit(0)
	  end if
	  vr(i)=v(i+1)/v(i)
	  if (v(i).gt.vm) vm=v(i)
	  g(i)=(v(i+1)-v(i))/thk(i)
  	  write (lprt,1006) i,v(i),z(i),thk(i),g(i)
	enddo
1006	format (1x,i2,3f6.2,f7.3)
	write (lprt,1007) lay,v(lay),z(lay)
1007	format (1x,i2,2f6.2,' halfspace')
	if (vm.ge.v(lay)) then
	  write (5,1013)
	  write (lprt,1013)
1013	  format (' fatal error: halfspace must have largest velocity')
	  call exit(0)
	end if
	g(lay)=0.
	vr(lay)=1.
	write (lprt,1016) redv
1016	format (/' one over reducing velocity=',f8.4)

c--calc and print out dist and depth grid point info
	nz=nz1+nz2+1
	write (lprt,1008) nz,maxz
1008	format (//1x,i3,' out of a possible',i4,' depth points used')
	if (nz.le.1 .or. nz.gt.maxz) then
	  write (5,1030)
	  write (lprt,1030)
1030	  format (' fatal error: improper number of depth points')
	  call exit(0)
	end if
	z1=nz1*dz1
	z2=z1+nz2*dz2
	i=nz1+1
	temp=0.

	write (lprt,1009) i,temp,z1
	temp=z1+dz2
	write (lprt,1009) nz2,temp,z2
1009	format (' calculate travel times at',i4,' depth points between',
	2 f6.2,' and',f6.2,' inclusive')
	nd=nd1+nd2+1
	write (lprt,1010) nd,maxd
1010	format (//1x,i3,' out of a possible',i4,' distance points used')
	if (nd.le.1 .or. nd.gt.maxd) then
	  write (5,1040)
	  write (lprt,1040)
1040	  format (' fatal error: improper number of distance points')
	  call exit(0)
	end if

	d1=nd1*dd1
	d2=d1+nd2*dd2
	i=nd1+1
	temp=0.
	write (lprt,1011) i,temp,d1
	temp=d1+dd2
	write (lprt,1011) nd2,temp,d2
1011	format (' calculate travel times at',i4,' distance points between'
	2 ,f7.2,' and',f7.2,' inclusive')

c--output the header info to the tt table file
	write (lout,1020) ititl,lay,redv,vpvs
1020	format (a20, i5, 2f10.5)
	write (lout,10211) (z(i),i=1,lay)
10211	format (' z ', 15f7.2)
	write (lout,1021) (v(i),i=1,lay)
1021	format (' v ', 15f7.2)
	write (lout,10221) dd1,nd1,dd2,nd2
10221	format (' dd', 2(f10.4,i5))
	write (lout,1022) dz1,nz1,dz2,nz2
1022	format (' dz', 2(f10.4,i5))

c+++++++++++++++++ depth loop +++++++++++++++++
c--loop includes foreward calculation, plotting tt curve, and interpolation
c  to get travel time table.
	do 700 m=1,nz
c--set depth as a function of m
	  zh=(m-1)*dz1
	  if (m-1.gt.nz1) zh=z1+(m-1-nz1)*dz2
c--set indicies at which shadow zone begins and ends
	  jc=0
	  jd=0
	  jl=0
	  jh=0
c--reset indicies where reverse branches begin and end
	  do i=1,10
	    ja(i)=0
  	    jb(i)=0
	  enddo

c--calc some depth dependent parameters
c--n is the layer in which hypo occurs
	  n=1
	  do i=1,lay
	    if (z(i).lt.zh) n=i
  	  enddo

c--velocity at hypocenter
	  vh=v(lay)
	  if (n.eq.lay) go to 25
	  vh=v(n)+g(n)*(zh-z(n))
c--maximum velocity above hypocenter
25	  vm=0.
	  do i=1,n
	    if (v(i).gt.vm) vm=v(i)
  	  enddo

c--now calculate distances and travel times as a function of ray parameter
	  call ttcal
c--plot out travel time curve on the tube
c	  call ttplt

c--interpolate tt curves to get a tt grid, and output the tt table
	  call ttgrd
700	continue
	write (lprt,1012)
1012	format (' ')
	close (lprt)
	close (lout)
	call exit(0)
	end
	block data
c--initializes constants used by the travel time table generating program
c--common variables for travel time table generating program ttgen
	parameter (maxz=28,maxd=42,maxl=20,maxj=201)
	character ititl*20,ivm*8
	common /ttc/ ititl,ivm
	common /ttcom/ z(maxl),v(maxl),g(maxl),thk(maxl),vr(maxl)
	common /ttcom/ tp(maxj),tr(maxj),dp(maxj),dmin,tmin
	common /ttcom/ dd1,nd1,dd2,nd2,lay,zh,vh,n,ja(10),jb(10),jc,jd,jh
	common /ttcom/ ibr,tmax,dmax,dq1,nq1,dq2,nq2,pi2,rdeg,vm
	common /ttcom/ linp,lprt,lout,redv,jl,vjl,vjc
	common /ttcom/ dz1,nz1,dz2,nz2,nz,nd,ihd,ipa
	data pi2,rdeg/1.57079,57.2958/
	data vm,linp,lprt,lout/0.,1,2,3/
	data dmin,tmin/2*0./
	end
	subroutine ttcal
c--calculates distance and travel time as a function of ray parameter
c--common variables for travel time table generating program ttgen

	parameter (maxz=28,maxd=42,maxl=20,maxj=201)
	character ititl*20,ivm*8
	common /ttc/ ititl,ivm
	common /ttcom/ z(maxl),v(maxl),g(maxl),thk(maxl),vr(maxl)
	common /ttcom/ tp(maxj),tr(maxj),dp(maxj),dmin,tmin
	common /ttcom/ dd1,nd1,dd2,nd2,lay,zh,vh,n,ja(10),jb(10),jc,jd,jh
	common /ttcom/ ibr,tmax,dmax,dq1,nq1,dq2,nq2,pi2,rdeg,vm
	common /ttcom/ linp,lprt,lout,redv,jl,vjl,vjc
	common /ttcom/ dz1,nz1,dz2,nz2,nz,nd,ihd,ipa

	dimension cp(15)
	character xmsg*4,cz*3,plfile*12

c--first treat the case of a ray travelling straight up (p=0)
	j=1
	p=0.
	q=0.
	phid=0.
	dp(1)=0.
	tp(1)=(zh-z(n))/v(n)
	if (g(n).ne.0.) tp(1)=alog(vh/v(n))/g(n)
	if (n.eq.1) go to 40
	do 35 i=1,n-1
	temp=thk(i)/v(i)
	if (g(i).ne.0.) temp=alog(vr(i))/g(i)
35	tp(1)=tp(1)+temp
40	tr(1)=tp(1)

c--open the travel time file in qplot plotting format
c  use the travel-time output filename, with a 3-letter extension
c  equal to the depth in 0.1 km. (one file for each depth)
	izh=zh*1.+.1
	livm=lentru(ivm)
	write (cz,1016) izh
1016	format (i3.3)
	plfile=ivm
	plfile(livm+1:livm+3)=cz
	open (8,file=plfile,form='formatted',status='new')

c--print heading and p=0 data before start of p loop
	write (lprt,1012) zh,n,vh
1012	format ('1depth=',f6.2,'   ... hypo in layer',i3,' at velocity'
	2 ,f6.2//'   j    q    em.ang    p    dist  ttime reduced l.bot'
	3 ,' z.bot v.bot  ddif  br   amp   amp*r**2   remk'/)
	write (lprt,1014) j,q,phid,p,dp(1),tp(1),tr(1)
1014	format (1x,i3,2f7.2,f7.4,3f7.2,i3,f9.2,f6.2,f7.2,i3,1pe9.2,
     *    f9.5,2x,a4)
	write (8,1015) dp(1),tp(1),tr(1)
1015	format (3f7.2)

c--set flags and indicies
c--set tt branch no. (no of reversals encountered +1)
	ibr=1
c--set flag (=1 if ray would go into lvz waveguide, =0 otherwise)
	ifl=0
c--set initial value of last phi (emergence angle) and last k
	phil=0.
	lastk=maxl

c++++++++++++ loop over ray parameter p ++++++++++++
c--use j as an integer index
c--use q=q(j) as an index which takes approximately equal steps in dist
c  on the surface (in km)
c--the emergence angle phi=phi(q) and ray parameter p=p(phi) are then calculated
	do 300 j=2,maxj

c--calc phi and other constants for the loop
	q=(j-1)*dq1
	if (j-1.gt.nq1) q=nq1*dq1+(j-nq1-1)*dq2
	phi=2.*atan(q/(zh+.5))
	sph=sin(phi)
	cph=sqrt(1.-sph**2)
	p=sph/vh
	vb=1./p

c--k is the layer in which downgoing ray bottoms
	do 41 i=1,lay-1
	k=i
	if (v(i+1) .ge. vb) go to 42
41	continue
42	if (vb.ge.v(lay)) k=lay

c--calc depth and velocity at ray bottom
	vbo=v(k)
	zb=z(k)
	if (g(k).ne.0.) then
	  zb=zb+(vb-v(k))/g(k)
	  vbo=1./p
	end if
	if (vb.gt.vm .or. vh.ge.vm) go to 45

c++++++++++++ ray lost to waveguide ++++++++++++
c--now have case of ray going into waveguide formed by a lvz
c--flag the nonexistent time and dist for this case
	tp(j)=-1.
	dp(j)=-1.
	tr(j)=-1.
	xmsg=' wg '
	k=0

c--skip ahead if ray was in waveguide on last pass too
	if (ifl.eq.1) go to 205
c--now have case of entering a shadow zone for the first time
c  so set flag
	ifl=1
c--record this place on tt curve for future reference and print a message
	jc=j-1
	vjc=vm
	write (lprt,1003) dp(jc),tr(jc),vjc
1003	format (' rays now entering waveguide. extend tt curve from',
	2 f7.2,' km,',f6.2,' sec at velocity',f5.2)
	go to 205
45	if (ifl.eq.0) go to 47

c--now have case of just emerging from a shadow zone
c--reset flag and record this place on tt curve
	ifl=0
	jd=j
47	if (phi.lt.pi2 .or. vb.lt.v(lay)) go to 55

c++++++++++++ ray into halfspace ++++++++++++
c--now have case of ray transmitted into halfspace.
c--such rays never reach the surface, but prog records time and dist for a ray
c  refracted from the top of the halfspace.
c--print a message and jump out of p loop
	jh=j-1
	write (lprt,1001) dp(jh),tr(jh),v(lay)
1001	format (' rays now lost to halfspace. extend tt curve from',
	2 f7.2,' km,',f6.2,' sec at velocity',f5.2)
	if (dd.lt.0.) jb(ibr)=jh

c--write a few points to the plot file for the refracted branch
	tox=tp(jh)
	dox=dp(jh)
	vox=v(lay)
	do 51 id=5,70,5
	temp=tox+id/vox
	demp=dox+id
	tremp=temp-demp*redv
	write (8,1015) demp,temp,tremp
51	continue
	go to 310

c++++++++++++ calculate time and distance ++++++++++++
c--calc t and d since it is valid to do so. consider all 3 cases:
c  1 ray nearly horiz in layer of zero gradient
c  2 upgoing ray
c  3 downgoing ray
c--for each term must consider zero and non-zero gradients as sep cases
55	if (abs(phi-pi2).gt..002 .or. g(n).ne.0.) go to 60

c--case of nearly horiz non-curving ray
c--set dist to be some arbitrary but large no
	dp(j)=1000.
	tp(j)=1000./v(n)
	go to 200
60	if (phi.gt.pi2+.0005) go to 90

c++++++++++++ upgoing ray ++++++++++++
c--calc cosines of emergence angles at each interface
	do 65 i=1,n
65	cp(i)=sqrt(1.-(v(i)*p)**2)
c--contribution to t and d of layer in which hypo lies
	if (g(n).eq.0.) go to 70
	dp(j)=(cp(n)-cph)/(p*g(n))
	tp(j)=alog(vh*(1.+cp(n))/(v(n)*(1.+cph)))/g(n)
	go to 75
70	dp(j)=(zh-z(n))*sph/cph
	tp(j)=(zh-z(n))/(cph*vh)

c--if hypo was in first layer, skip this loop
75	if (n.eq.1) go to 200
c--now add terms from overlying layers
	do 85 i=1,n-1
	if (g(i).eq.0.) go to 80
	dp(j)=dp(j)+(cp(i)-cp(i+1))/(p*g(i))
	tp(j)=tp(j)+alog(vr(i)*(1.+cp(i))/(1.+cp(i+1)))/g(i)
	go to 85
80	dp(j)=dp(j)+thk(i)*p*v(i)/cp(i)
	tp(j)=tp(j)+thk(i)/(cp(i)*v(i))
85	continue
	go to 200

c++++++++++++ downgoing ray ++++++++++++
c--calc cosines of emergence angles at layer interfaces
90	do 95 i=1,k
95	cp(i)=sqrt(1.-(v(i)*p)**2)
c--contribution to t and d from layer in which ray bottoms
	if (g(k).eq.0.) go to 100
	dp(j)=2.*cp(k)/(p*g(k))
	tp(j)=2.*alog((1.+cp(k))/(v(k)*p))/g(k)
	go to 105

c--a ray cant bottom in a homogeneous layer
c--if this case should arise, reflect ray from top of the layer
100	dp(j)=0.
	tp(j)=0.
c--subtract contribution from path immed above hypo in its layer
105	if (g(n).eq.0.) go to 110
	dp(j)=dp(j)-(cp(n)-cph)/(p*g(n))
	tp(j)=tp(j)-alog(vh*(1.+cp(n))/(v(n)*(1.+cph)))/g(n)
	go to 115
110	dp(j)=dp(j)-(zh-z(n))*sph/cph
	tp(j)=tp(j)-(zh-z(n))/(cph*vh)

c--sum terms over layers above hypo
115	if (n.eq.1) go to 130
	do 125 i=1,n-1
	if (g(i).eq.0.) go to 120
	dp(j)=dp(j)+(cp(i)-cp(i+1))/(p*g(i))
	tp(j)=tp(j)+alog(vr(i)*(1.+cp(i))/(1.+cp(i+1)))/g(i)
	go to 125
120	dp(j)=dp(j)+thk(i)*p*v(i)/cp(i)
	tp(j)=tp(j)+thk(i)/(cp(i)*v(i))
125	continue

c--sum terms over layers between hypo and ray bottom (if any)
c--include layer in which hypo lies
130	if (n.eq.k) go to 150
	do 140 i=n,k-1
	if (g(i).eq.0.) go to 135
	dp(j)=dp(j)+2.*(cp(i)-cp(i+1))/(p*g(i))
	tp(j)=tp(j)+2.*alog(vr(i)*(1.+cp(i))/(1.+cp(i+1)))/g(i)
	go to 140
135	dp(j)=dp(j)+2.*thk(i)*p*v(i)/cp(i)
	tp(j)=tp(j)+2.*thk(i)/(cp(i)*v(i))
140	continue

c--print a message and record this place if this downgoing ray 
c  has emerged from the far side of a shadow zone
150	if (k-lastk.lt.2 .or. lastk.eq.0) go to 200
	if (k.lt.2) go to 200
	if (g(k-1).ge.0.) go to 200
	vjl=v(lastk+1)
	jl=j-1
	write (lprt,1002) dp(jl),tr(jl),vjl
1002	format (' downgoing rays jump over a shadow zone. extend tt curve'
	2 ,' from',f7.2,' km,',f6.2,' sec at velocity',f5.2)

c++++++++++++ prepare to print results ++++++++++++
c--calc some extra parameters and prepare to print results
c--come here for a normal ray which reaches the surface
200	xmsg='    '
c--come here if ray went into waveguide
205	if (dp(j).ne.-1..and. dp(j-1).ne.-1.) go to 215
	dd=0.
	if (j.eq.jd) dd=dp(jd)-dp(jc)
	amp=0.
	ampd=0.
	go to 220

c--come here for a normal ray which reached the surface both this time and last
215	dd=dp(j)-dp(j-1)+.001
c--flag reversed branch rays
	if (dd.lt.0.) xmsg=' rb '

c--calc a parameter proportional to amplitude to estimate geometrical spreading
c  and also to correct it for inverse square factor
	if (abs(dd).lt..00001) dd=.00001
c--be sure not to divide by zero or a negative no.
	amp=sph*(phi-phil)/(dp(j)*abs(dd)+.00001)
	ampd=amp*(dp(j)**2+zh**2)
	if (j.eq.2) go to 220

c--record index if reversal to another tt branch has taken place
	if (dd.lt.0. .and. ddl.gt.0.) ja(ibr)=j-1
	if (dd.lt.0. .or. ddl.gt.0.) go to 220
	jb(ibr)=j-1
	ibr=ibr+1
c--record parameters for comparason on next pass of loop
220	ddl=dd
	lastk=k
	phil=phi
	phid=phi*rdeg
c--calc reduced travel time
	if (dp(j).ne.-1.) tr(j)=tp(j)-dp(j)*redv

c--print out 1 line of results
	write (lprt,1014) j,q,phid,p,dp(j),tp(j),tr(j),k,zb,vbo,dd,ibr,amp
	2 ,ampd,xmsg
c--write a point on travel time curve to plot file
	write (8,1015) dp(j),tp(j),tr(j)
c--end of p loop
300	continue

c--be sure that travel times are defined for large distances
c  if halfspace has not yet been reached
	jh=maxj
c--write out flagging indicies
310	i=0
	if (jl.gt.0) i=jl+1
	close (8)
	write (lprt,1000) jc,jd,jl,i,ja,jb
1000	format (/' j indicies of ends of shadow zone (hypo in lvz):',2i4//
	2 ' j indicies of ends of shadow zone (hypo above lvz):',2i4//
	2 ' j indicies of ends of reverse branches:',2(/10i4))
	return
	end
	subroutine ttgrd
c--interpolates and outputs a single continuous travel time curve
c--from the earliest arriving branches of the tp vs. dp curve.
c--common variables for travel time table generating program ttgen

	parameter (maxz=28,maxd=42,maxl=20,maxj=201)
	character ititl*20,ivm*8
	common /ttc/ ititl,ivm
	common /ttcom/ z(maxl),v(maxl),g(maxl),thk(maxl),vr(maxl)
	common /ttcom/ tp(maxj),tr(maxj),dp(maxj),dmin,tmin
	common /ttcom/ dd1,nd1,dd2,nd2,lay,zh,vh,n,ja(10),jb(10),jc,jd,jh
	common /ttcom/ ibr,tmax,dmax,dq1,nq1,dq2,nq2,pi2,rdeg,vm
	common /ttcom/ linp,lprt,lout,redv,jl,vjl,vjc
	common /ttcom/ dz1,nz1,dz2,nz2,nz,nd,ihd,ipa

	dimension kt(101),cp(maxl)
	data scfac/2000./

c--define linear interpolation function
	tlin(d,i)=tr(i-1)+(d-dp(i-1))*(tr(i)-tr(i-1))/
	2 (dp(i)-dp(i-1))
	xmax=0.
c--set tt at zero distance
	kt(1)=tr(1)*scfac-32000.

c+++++++++++++ distance loop +++++++++++++++++++++++++
c--for each distance point find the smallest travel time from
c--the several possible branches and extensions
	do 90 l=2,nd
	tmn=1000.
c--calc distance from constants specified
	d=(l-1)*dd1
	if (l-1.gt.nd1) d=nd1*dd1+(l-nd1-1)*dd2

c--tmn will be reset if the reduced tt of some branch is less than
c--the least so far
c--test tt interpolated from first foreward branch of tt curve
	if (ja(1).eq.0) go to 11
	if (d.lt.dp(ja(1))) go to 12
	go to 19
11	lim=jh
	go to 14
12	lim=ja(1)
14	do 15 ii=2,lim
	i=ii
	if (d.lt.dp(i)) go to 17
15	continue
16	if (dp(i).ne.0.) go to 17
	t=tr(i)
	go to 18
17	t=tlin(d,i)
18	if (t.lt.tmn) tmn=t

c--test tt defined as extension thru a shadow zone (hypo in lvz)
19	if (jc.eq.0) go to 20
	if (d.le.dp(jc)) go to 20
	t=tp(jc)+(d-dp(jc))/vjc-d*redv
	if (t.gt.tmn) go to 20
	tmn=t

c--test tt defined as extension thru a sz (hypo above a lvz)
20	if (jl.eq.0) go to 30
	if (d.le.dp(jl)) go to 30
	t=tp(jl)+(d-dp(jl))/vjl-d*redv
	if (t.gt.tmn) go to 30
	tmn=t

c--test tt defined as refraction from halfspace
30	if (d.le.dp(jh)) go to 50
	t=tp(jh)+(d-dp(jh))/v(lay)-d*redv
	if (t.gt.tmn) go to 50
	tmn=t

c--test tt interpolated from last foreward branch of tt curve
50	if (ibr.lt.2) go to 80
	if (d.gt.dp(ja(ibr))) go to 80
	if (d.lt.dp(jb(ibr-1))) go to 60
	do 53 ii=jb(ibr-1)+1,ja(ibr)
	i=ii
	if (d.lt.dp(i)) go to 55
53	continue
55	t=tlin(d,i)
	if (t.gt.tmn) go to 60
	tmn=t

c--test tt interpolated from foreward branches of tt curve
c  with reversals at both ends
60	if (ibr.lt.3) go to 80
	do 70 j=1,ibr-2
	if (d.gt.dp(ja(j+1)) .or. d.lt.dp(jb(j))) go to 70
	do 63 ii=jb(j)+1,ja(j+1)
	i=ii
	if (d.lt.dp(i)) go to 65
63	continue
65	t=tlin(d,i)
	if (t.gt.tmn) go to 70
	tmn=t
70	continue

c--record earliest arrival time
80	kt(l)=tmn*scfac-32000.
c--record distance at which a horiz ray reaches the surface
c--the dist is where a max value of dtdr occurs
	temp=dd1
	if (l-1.gt.nd1) temp=dd2
	tmn=(kt(l)-kt(l-1))/temp
	if (tmn.lt.xmax) go to 90
	xmax=tmn
	kdhr=(d-temp*.5)*10.
90	continue
	if (zh.gt.z(lay)) kdhr=10000

c--use a better method to recalculate kdhr unless a horiz ray never emerges
        if (g(n).eq.0. .or. vh.le.vm) goto 95
        p=1./vh
c--calc cosines of emergence angles at each interface
        do i=1,n
          cp(i)=sqrt(1.-(v(i)*p)**2)
        end do
c--contribution to d of layer in which hypo lies
        d=cp(n)/(p*g(n))
c--add terms from overlying layers
        do i=1,n-1
          if (g(i).eq.0.) then
            d=d+thk(i)*p*v(i)/cp(i)
          else
            d=d+(cp(i)-cp(i+1))/(p*g(i))
          end if
        end do
        kdhr=d*10.

c--output the final tt curve
95	write (lout,1000) zh,vh,kdhr,(kt(l),l=1,nd)
1000	format (2f10.4,i10/(15i8))
	return
	end
c
c AASK prompts on LUNOUT=5, then reads a real value from
c LUNIN=0 using A80 format and a default value of dflt.
c
c ORIGINAL VERSION WRITTEN BY LARRY BAKER AND EXTENSIVELY MODIFIED BY J 
c LAHR AND C STEPHENS.
c
c *** Machine dependent CODE (WORKS ON PDP VAX): ***
c *** $ and Q Format descriptors ***
c
c val = AASK (prompt,dflt,lenf)
c
c       val    = CHARACTER STRING response.
c       prompt = Prompt string.
c       dflt   = Default supplied for carriage return response
c       lenf   = If positive, then right justify response in first
c                  lenf characters
c                If negative, left justify in field of length iabs(lenf)
c
      character *(*)function aask(prompt, dflt, lenf)
      parameter (lunin = 5)
      parameter (lunout = 0)
      character prompt*(*)
      character dflt*(*), fmt*45, inline*200
c
      logical eof
1     lenfld = iabs(lenf)
      lenaask = len(aask)
      aask = ' '
      if (lenfld .le. lenaask) goto 5
      write(unit=lunout, fmt=4) lenfld, lenaask
    4 format(1x,35h  ** Warning -- character length of,i3,10h specified
     &,/17h but truncated to,i3,
     &39h, the specified length of FUNCTION AASK)
      lenfld = lenaask
    5 if (prompt .ne. ' ') then
      lendflt = lentru(dflt)
      if (lendflt .eq. 0) lendflt = 1
      if (lendflt .gt. lenaask) lendflt = lenaask
      fmt = '(1H , A, 3H [A,I1,4H;CR=, /, 1X, A, 3H]? , $)'
      if ((lenfld .gt. 0) .and. (lenfld .lt. 10)) goto 505
      if ((lenfld .gt. 9) .and. (lenfld .lt. 100)) then
      fmt(17:17) = '2'
      goto 505
      end if
      if (lenfld .gt. 99) then
      fmt(17:17) = '3'
      goto 505
      end if
  505 if ((lendflt + len(prompt)) .lt. 69) fmt(27:28) = '  '
      write(unit=lunout, fmt=fmt) prompt(1:len(prompt)), lenfld, dflt(1:
     &lendflt)
c
      end if
      read(unit=lunin, fmt=504, end=9900, err=9100) inline
  504 format(a)
      nch = lentru(inline)
      eof = .false.
c
      if (nch .gt. 0) goto 9000
 8000 inline = dflt
      nch = lentru(dflt)
c
      if (nch .eq. 0) nch = 1
c      Right justify
c      Put rightmost nonblank character in position lenfld
 9000 if (lenf .gt. 0) then
      i = lentru(inline)
      if (i .eq. 0) then
      aask = ' '
      return 
      end if
 9070 ifrst = (i - lenfld) + 1
      ist = (- ifrst) + 2
      if (ist .lt. 1) ist = 1
      if (ifrst .lt. 1) ifrst = 1
      aask(ist:lenfld) = inline(ifrst:i)
      if (ist .ne. 1) aask(1:ist - 1) = ' '
c      Left justify
      else
      i = ibegtru(inline)
      aask(1:lenfld) = inline(i:nch)
      end if
c
 9085 return 
 9100 continue
      write(unit=lunout, fmt=9110) 
 9110 format(1h0,30h  ** Error in FUNCTION AASK --,
     &23h Input line is too long/)
c
      goto 5
 9900 eof = .true.
      goto 8000
      end
c lentru.for    []
      integer function lentru(alph)
c     finds the true length of a character variable
      character alph*(*)
      l = len(alph)
      do 100 i = l, 1, -1
       if(alph(i:i).ne.' ' .and. alph(i:i).ne.'\0') then
         lentru = i
         return
        endif
100   continue
      lentru = 0
      return
      end
c end lentru
