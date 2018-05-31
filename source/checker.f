	character*120 fname

	include 'sum.inc'
	include 'phase.inc'
	include 'scan.inc'
	include 'station.inc'

	data luni/2/
	data luno/6/

C	open(unit=luno, file='checker.MSG', status='unknown', iostat=ios)
C	rewind(unit=luno)
	write(luno,'('' '')')

	nargs = iargc()

	if (nargs .eq. 0) go to 90

	call getarg(1, fname)

	open(unit=luni, file=fname, status='old', iostat=ios)
	if (ios .ne. 0) then
	  write(luno,'(''Error opening phase file: '',a)') fname(:lastc(fname))
	  go to 90
	endif

	ires = loadph(luni,luno)
	close (unit=luni)
	if (ires .lt. 0) go to 90

c	print *, 'loadph ires =', ires
c	print *, '  nsum, npr, nscan:', nsum, npr, nscan

c    load a list of station parameters

	open(unit=luni, file=stfile, status='old', form='unformatted',
     *		iostat=ios)
	nmst = 0
	do while (ios .eq. 0 .and. nmst .lt. mxsta)
	  nmst = nmst + 1
	  read(2,iostat=ios) stcode(nmst), stlat(nmst), stlon(nmst),
     *		stexp(nmst)
	enddo
	if (ios .eq. -1) nmst = nmst - 1
	close(unit=luni)


	ires = icheck(luno)
c	print *, 'icheck ires =', ires

90 	print *, 'Leaving checker'
	continue
	end
