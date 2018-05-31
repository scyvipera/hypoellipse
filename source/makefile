# For Linux:  Change -Bstatic to -static

# For solaris use the following lines:
FC= gfortran -C -O
CC= cc -O
CLIBS = -lnsl

# For OS 4.1.* use the following lines:
#FC= f77 -C -O  -DSUN
#CC= cc -O  -DSUN
#CLIBS = -lF77 -lm

# OBJS_XPICK are required for running hypoellipse with xpick.
OBJS_XPICK =	hypoe.o\
	cleanup.o\
	fdgetstr.o\
	initial.o\
	listen_serv.o\
	setup_server.o

OBJS_NO_XPICK =	hymain.o\
	init.o

OBJS =	adddly.o\
	adderr.o\
	azwtos.o\
	back.o\
	block.o\
	boxau.o\
	cosh.o\
	coshi.o\
	critic.o\
	cvrtop.o\
	cyldly.o\
	cylget.o\
	cylred.o\
	delaz.o\
	diftim.o\
	dnstrg.o\
	dubl.o\
	dwnwrd.o\
	eigen1.o\
	erset.o\
	fmplot.o\
	fmplt.o\
	fold2.o\
	formal.o\
	formf.o\
	formit.o\
	getbin.o\
	geterr.o\
	getsta.o\
	global.o\
	glob_new.o\
	halfsp.o\
	hycrt.o\
	hypot.o\
	hyset.o\
	hytab.o\
	input1.o\
	inside.o\
	iprst.o\
	jdate.o\
	ksicr.o\
	lentru.o\
	line3.o\
	linv.o\
	linvol.o\
	lisinc.o\
	lissum.o\
	locate.o\
	median.o\
	mising.o\
        openfl.o\
	phagt.o\
	phaind.o\
	prplot.o\
	qdist.o\
	quakes.o\
	ran3.o\
	rdtmdp.o\
	redgap.o\
	regres.o\
	riorbk.o\
	rnd.o\
	rplain.o\
	rshft.o\
	sinh.o\
	sort.o\
	strtep.o\
	sumgap.o\
	tatime.o\
	tbtime.o\
	tdtime.o\
	timit.o\
	trvcon.o\
	trvdrv.o\
	tshift.o\
	uamag.o\
	unfold2.o\
	update.o\
	upstrg.o\
	upward.o\
	usedly.o\
	velazm.o\
	wadati.o\
	weight.o\
	xfmags.o\
	zstats.o\
	npunch.o\
	opfls.o\
	output.o\
	phasin.o

Hypoel: $(OBJS_NO_XPICK) $(OBJS) params.inc
	$(FC) -static $(OBJS_NO_XPICK) $(OBJS) -o Hypoellipse

Hypoel_xpick: $(OBJS_XPICK) $(OBJS) params.inc
	$(FC) $(OBJS_XPICK) $(OBJS) $(CLIBS) -o Hypoellipse_xpick

Y2KSTA=	y2ksta.o

y2ksta: $(Y2KSTA)
	$(FC) $(Y2KSTA) -o y2ksta

clean:
	/bin/rm *.o Hypoel Hypoel_xpick
