c tshift.for    []
      subroutine tshift(kdate,khr,kmin,sec,ssec)
c resolve unreasonable times
    5 if (sec .lt. 60.0) goto 20
      sec = sec - 60.0
      ssec = ssec - 60.0
      kmin = kmin + 1
      goto 5
   20 if (sec .ge. 0.0) goto 40
      sec = sec + 60.0
      ssec = ssec + 60.0
      kmin = kmin - 1
      goto 20
   40 if (kmin .lt. 60) goto 60
      kmin = kmin - 60
      khr = khr + 1
      goto 40
   60 if (kmin .ge. 0) goto 80
      kmin = kmin + 60
      khr = khr - 1
      goto 60
   80 if (khr .lt. 25) goto 100
      khr = khr - 24
      kdate = kdate + 1
      goto 80
  100 if (khr .ge. 0) goto 120
      khr = khr + 24
      kdate = kdate - 1
      goto 100
  120 return
      end
c end tshift
