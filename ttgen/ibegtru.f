c     FINDS THE TRUE START OF A CHARACTER VARIABLE
      integer function ibegtru(alph)
      character alph*(*)
      l = len(alph)
      do 100 i = 1, l
      if ((alph(i:i) .ne. ' ') .and. 
     *    (ichar(alph(i:i)) .ne. 9)) goto 200
  100 continue
      ibegtru = 1
      return 
  200 ibegtru = i
      return 
      end
