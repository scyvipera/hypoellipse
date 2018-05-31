c hymain.for    []
      program hypoel
c hypoellipse -- john c. lahr
c non-unix version of main routine 
c
c************************* notes for programmers **************************
c
c     a binary search of the station list is used in the function phaind.
c         if the search does not work on your computer, use the version
c         of phaind that is commented out, which does a complete search.
c
c     subroutines init, opfls, trvcon, trvdrv, and uamag must have 
c     $notruncate statements added for pc systems so that variable names 
c     longer than 6 characters may be used.
c     
c     all non-unix systems use hymain.for and init.for.  the equivalent
c         routines for unix systems are hypoe.c, setup_server.c, 
c         listen_serv.c, fdgetstr.c, cleanup.f, initial.f, and
c         getbin.f.  
c
c     subroutines openfl and opfls, which open files, have differences between
c         various systems. 
c
c     subroutine openfl also has differences between the unix-masscomp and
c         the unix-sun systems.
c
c     subroutines phagt and npunch use a back slash character, which 
c         must be doubled on unix systems.  
c
c     subroutines dubl, erset, jdate, and timit use non-standard fortran 
c         and must be modified for use with vax, pc, or unix computers.
c
c           dubl     - sets a double precision number equal to a 
c                        single precision number
c           erset    - resets error limits on vax/vms computers
c           jdate    - gets current date and time from the operating system
c           timit    - times the execution on vax/vms computers
c
c     alternat versions of the code is enclosed by 'c* unix', 'c* pc', 
c         or 'c* vax' comment statements in each of the above subroutines.
c

c get filenames, open files and write greeting
      intype = 1
      call init

c read station list, crustal model, and control records
      call input1

c initialize summary of residuals
      call lissum(1)

c read arrival times and locate earthquakes
      call locate(-1)
      stop
      end
c end hymain
