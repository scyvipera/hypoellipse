UNIX Version of HYPOELLIPSE

This directory contains source and executable files for HYPOELLIPSE, examples of two program runs, 
files for creating local travel-time tables for use with HYPOELLIPSE, files for reformatting 
University of Alaska station calibrations, and code for updating the format of an old station list.  

The following is a list of sub-directories with a description of their contents:

source
	Source files for HYPOELLIPSE and Makefile for compiling the source components.  The latest 
	revision to the source files was made on 2/9/2000.  

hypoel
	UNIX-executable HYPOELLIPSE file.

	Note that Makefile for the SUN version allows two different versions of HYPOELLIPSE to be 
	compiled.  One works with Xpick through sockets; the other is the stand-alone version. The 
	executable code included here is the non-Xpick version and was compiled with the -Bstatic 
	option so that FORTRAN libraries will not be needed to run it.

quickrun
	An example of a HYPOELLIPSE test run as described in the Quick-Start Manual.

samplerun
	An example of a final HYPOELLIPSE run on Alaska data.

ttgen
	Files to compile and run TTGEN, a program for creating local, flat-earth, travel-time tables 
	for use with HYPOELLIPSE.

uamag
	Files for reformatting calibration files for University of Alaska-style magnitude computations.

y2ksta
	Source and executable codes for updating the format of an old station list with time-dependent 
	parameters that was in use before 1999.


