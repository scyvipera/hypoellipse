c upstrg.for    []
      character*(*) function upstrg(array)
c
c  program to change string to uppercase
c
c  array - a character variable
      character*(*) array
      integer offset
      data offset/32/
c
c  get length of array
c
      upstrg = ' '
      lenstr = len(array)
      if (lenstr .eq. 0) return
      do 10 i = 1, lenstr
        ic = ichar(array(i:i))
        if ((ic .ge. 97) .and. (ic .le. 122)) then
          upstrg(i:i) = char(ic - offset)
        else
          upstrg(i:i) = array(i:i)
        endif
   10 continue
      return
      end
c end upstrg
