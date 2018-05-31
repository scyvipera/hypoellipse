      program main
c get_start_date reads a hypoellipse phase file on standard
c input and writes the date of the first phase record on
c standard output.   jcl  8/1/89

      character*81 record
20    read(5, '(a)', END=30) record
      if((record(81:81) .eq. '/') .or.
     *   (record(81:81) .eq. '\\') .or. 
     *   (record(1:2) .eq. 'c*') .or.
     *   (record(1:2) .eq. 'C*') .or.
     *   (record(10:15) .eq. '      ')) goto 20
      write(*, '(1x,a)') record(10:15)
      stop
30    write (*,'(" 650101")')
      end
