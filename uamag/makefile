CFLAGS = -g
FFLAGS = -g

OBJS = get_uacal.o getbin_uacal.o put_uacal.o putbin_uacal.o squish_uacal.o 

TOOLS = lentru.o openfl.o upstrg.o rshft.o

all: bin2prm_cal prm2bin_cal

bin2prm_cal: bin2prm_cal.o $(OBJS) $(TOOLS)
	gfortran -o bin2prm_cal bin2prm_cal.o $(OBJS) $(TOOLS)

prm2bin_cal: prm2bin_cal.o $(OBJS) $(TOOLS)
	gfortran -o prm2bin_cal prm2bin_cal.o $(OBJS) $(TOOLS)

fix_uacal: fix_uacal.o $(OBJS) $(TOOLS)
	gfortran -o fix_uacal fix_uacal.o $(OBJS) $(TOOLS)

convert_oldcal: convert_oldcal.o get_olduacal.o put_uacal.o squish_uacal.o $(TOOLS)
	gfortran -o convert_oldcal convert_oldcal.o get_olduacal.o put_uacal.o \
	squish_uacal.o $(TOOLS)

fixsdn:  fixsdn.o
	gfortran fixsdn.o -o fixsdn -lvaxlib 


